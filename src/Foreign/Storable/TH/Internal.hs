module Foreign.Storable.TH.Internal (roundUp) where

-- | Round the first parameter to a multiple of the second parameter.
--
-- Examples:
-- > roundUp 5 4
-- 8
-- > roundUp 14 4
-- 16
roundUp :: Int -> Int -> Int
roundUp num 0 = num
roundUp num mult
  | remainder == 0 = num
  | otherwise = num + mult - remainder
 where
  remainder = num `rem` mult
