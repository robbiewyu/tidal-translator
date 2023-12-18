module TidalToLilyCheck where

import Data.Music.Lilypond as L
import Sound.Tidal.ParseBP (TPat)
import Test.HUnit
import Test.QuickCheck as QC (Arbitrary (..), quickCheck, withMaxSuccess)
import TidalToLily (tidalToLilypond)

instance Arbitrary TPat where
  arbitrary = sized arbitraryTPat

arbitraryTPat :: (Arbitrary a) => Int -> Gen (TPat a)
arbitraryTPat 0 = do
  val <- arbitrary
  return (TPat_Atom Nothing val)
arbitraryTPat n
  | n > 0 = do
      let n' = n `div` 2
      oneof
        [ arbitraryTPatAtom n',
          arbitraryTPatFast n',
          arbitraryTPatSlow n'
          -- Add cases for other constructors here
        ]
  | otherwise = arbitraryTPat 0

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheck . QC.withMaxSuccess n

prop_timeSigCorrect :: [ArcF Time] -> L.Music -> Bool
prop_timeSigCorrect subdivisions music =
  -- make sure that the smallest common denominator of all the subdivisions
  -- is the length of the top level time signature
  let denominators = map (\(Arc start finish) -> denominator start) subdivisions
      largestCommonDenominator = foldl lcm 1 denominators
   in case music of
        Time num den -> num == largestCommonDenominator
        _ -> False

qc :: IO ()
qc = do
  quickCheck prop_timeSigCorrect