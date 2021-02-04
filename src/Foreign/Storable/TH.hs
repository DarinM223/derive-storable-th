{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Foreign.Storable.TH where

import Prelude hiding (exp)
import Data.Foldable (foldl')
import Foreign.Storable (Storable (..))
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

  sizeOf' :: [BangType] -> Q Exp
  sizeOf' = foldl' build [| 0 |]
   where build acc t = [| $(toSizeOf t) + $acc |]

  alignment' :: [BangType] -> Q Exp
  alignment' [] = [| 0 |]
  alignment' ts = [| maximum $(foldr cons [| [] |] $ fmap toSizeOf ts) |]

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
     where offset' = [| $offset + $(toSizeOf t) |]

  poke' :: Name -> [BangType] -> Q Exp
  poke' _ []   = LamE [WildP, WildP] <$> [| return () |]
  poke' con ts = do ptr <- newName "ptr"
                    t <- newName "t"
                    LamE [VarP ptr, VarP t] <$> poke'' ptr t con ts

  poke'' :: Name -> Name -> Name -> [BangType] -> Q Exp
  poke'' ptr t con ts0 = do
    names <- traverse (\_ -> newName "temp") ts0
    let initData =
          ([| 0 |], [[| pokeByteOff $(varE ptr) 0 $(varE (head names)) |]])
        (_, exps) = foldl' build initData (zip (tail names) (init ts0))
    doE $ [bindS (conP con (varP <$> names)) [| pure $(varE t) |]]
       <> (noBindS <$> exps)
   where
    build (!offset, !l) (n, ty) = (offset', exp:l)
     where
      offset' = [| $offset + $(toSizeOf ty) |]
      exp = [| pokeByteOff $(varE ptr) $offset' $(varE n) |]
