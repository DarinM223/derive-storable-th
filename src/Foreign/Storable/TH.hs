{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Foreign.Storable.TH where

import Prelude hiding (exp)
import Foreign.Storable (Storable (..))
import Foreign.Storable.TH.Internal (roundUp)
import Language.Haskell.TH

deriveStorable :: Name -> Q [Dec]
deriveStorable name = do
  TyConI tyCon <- reify name
  con <- case tyCon of
    DataD _ _ _ _ [c] _  -> pure c
    NewtypeD _ _ _ _ c _ -> pure c
    _                    -> do
      reportError $ "Type " ++ show name ++
        " not single constructor data or newtype"
      return $ NormalC name []
  (conName, ts) <- case con of
    NormalC name' ts' -> pure (name', ts')
    RecC name' ts'    -> pure (name', (\(_, a, b) -> (a, b)) <$> ts')
    _                 -> do
      reportError "Constructor not record or normal constructor"
      return (name, [])
  [d|
    instance Storable $(conT name) where
      sizeOf _ = $(sizeOf' ts)
      alignment _ = $(alignment' ts)
      peek = $(peek' conName ts)
      poke = $(poke' conName ts)
    |]
 where
  cons e acc = [| $e : $acc |]
  toSizeOf (_, t) = [| sizeOf (undefined :: $(pure t)) |]
  toAlignment (_, t) = [| alignment (undefined :: $(pure t)) |]

  sizeOf' :: [BangType] -> Q Exp
  sizeOf' ts = foldl' build [| 0 |] ts
   where build acc t = [| roundUp $(toSizeOf t) $(alignment' ts) + $acc |]

  alignment' :: [BangType] -> Q Exp
  alignment' [] = [| 0 |]
  alignment' ts = [| maximum $(foldr cons [| [] |] $ fmap toAlignment ts) |]

  peek' :: Name -> [BangType] -> Q Exp
  peek' con [] = LamE [WildP] <$> [| return $(conE con) |]
  peek' con ts = do ptr <- newName "ptr"
                    LamE [VarP ptr] <$> peek'' ptr con ts

  peek'' :: Name -> Name -> [BangType] -> Q Exp
  peek'' ptr con ts0 =
    go [| 0 |] [| $(conE con) <$> peekByteOff $(varE ptr) 0 |] (init ts0)
   where
    go _ expr [] = expr
    go !offset !expr (t:ts) =
      go offset' [| $expr <*> peekByteOff $(varE ptr) $offset' |] ts
     where offset' = [| $offset + roundUp $(toSizeOf t) $(alignment' ts0) |]

  poke' :: Name -> [BangType] -> Q Exp
  poke' _ []   = LamE [WildP, WildP] <$> [| return () |]
  poke' con ts = do ptr <- newName "ptr"
                    t <- newName "t"
                    LamE [VarP ptr, VarP t] <$> poke'' ptr t con ts

  poke'' :: Name -> Name -> Name -> [BangType] -> Q Exp
  poke'' ptr t con ts0 = do
    names <- traverse (\_ -> newName "temp") ts0
    case names of
      hd : tl -> do
        let initData =
              ([| 0 |], [[| pokeByteOff $(varE ptr) 0 $(varE hd) |]])
            (_, exps) = foldl' build initData (zip tl (init ts0))
        doE $ [bindS (conP con (varP <$> names)) [| pure $(varE t) |]]
           <> (noBindS <$> exps)
      _ -> error "Not enough names"
   where
    build (!offset, !l) (n, ty) = (offset', exp:l)
     where
      offset' = [| $offset + roundUp $(toSizeOf ty) $(alignment' ts0) |]
      exp = [| pokeByteOff $(varE ptr) $offset' $(varE n) |]
