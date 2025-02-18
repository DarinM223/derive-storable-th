module Foreign.Storable.Internal (nearestPowerOfTwo) where

import Data.Bits

nearestPowerOfTwo :: Int -> Int
nearestPowerOfTwo v = v6 + 1
 where
  v6 = v5 .|. v5 `shiftR` 16
  v5 = v4 .|. v4 `shiftR` 8
  v4 = v3 .|. v3 `shiftR` 4
  v3 = v2 .|. v2 `shiftR` 2
  v2 = v1 .|. v1 `shiftR` 1
  v1 = v - 1
