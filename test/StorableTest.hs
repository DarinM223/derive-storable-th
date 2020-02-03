{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Foreign.Storable.TH

data Test = Test
  { testA :: !Int
  , testB :: !Int
  , testC :: !Float
  , testD :: !Double
  , testE :: !Int
  } deriving Eq
$(deriveStorable ''Test)

main :: IO ()
main = putStrLn "Test suite not yet implemented."
