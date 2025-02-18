module Foreign.Storable.Internal (nearestPowerOfTwo, roundUp, tr) where

import Data.Bits
import Debug.Trace

nearestPowerOfTwo :: Int -> Int
nearestPowerOfTwo v = v6 + 1
 where
  v6 = v5 .|. v5 `shiftR` 16
  v5 = v4 .|. v4 `shiftR` 8
  v4 = v3 .|. v3 `shiftR` 4
  v3 = v2 .|. v2 `shiftR` 2
  v2 = v1 .|. v1 `shiftR` 1
  v1 = v - 1

roundUp :: Int -> Int -> Int
roundUp num 0 = num
roundUp num mult
  | remainder == 0 = num
  | otherwise = num + mult - remainder
 where
  remainder = num `rem` mult

tr :: Show a => String -> a -> a
tr desc a = trace (desc ++ ": " ++ show a) a