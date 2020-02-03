{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Foreign.Storable.TH
import Test.HUnit (assertEqual)
import qualified Data.Vector.Storable as V

data Test = Test
  { testA :: !Int
  , testB :: !Int
  , testC :: !Float
  , testD :: !Double
  , testE :: !Int
  } deriving (Show, Eq)
$(deriveStorable ''Test)

main :: IO ()
main = assertEqual "Storable failed" (V.toList (V.fromList d)) d
 where
  d = take 100 $ cycle
    [ Test 9 10 9.8 10.3 50
    , Test (-600) 1000 6083021.2341 134729184732.41894321 143278
    ]
