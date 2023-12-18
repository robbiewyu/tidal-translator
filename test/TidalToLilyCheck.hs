module TidalToLilyCheck where

import Data.Music.Lilypond as L
import Sound.Tidal.ParseBP (TPat (..))
import Test.HUnit
import Test.QuickCheck as QC (Arbitrary (..), Gen, quickCheck, sample', shrinkNothing, suchThat, withMaxSuccess)
import TidalToLily

instance Arbitrary (TPat Rational) where
  arbitrary = TPat_Atom Nothing <$> positiveRational
    where
      positiveRational = QC.arbitrary `suchThat` (> 0)
  shrink = QC.shrinkNothing

prop :: TPat Rational -> Bool
prop tpat = case tpat of
  TPat_Atom Nothing _ -> True
  _ -> False

-- Property tests
-- 1) Smallest common denominator of all subdivisions is the length of the top level time signature
-- 2) Number of notes in the lilypond output is the same as the number of notes in the tidal pattern
-- 3)

-- genTPat :: Gen (TPat Rational)
-- genTPat = return $ TPat_Atom Nothing (QC.arbitrary :: Gen Rational)

-- arbitraryTPat :: (Arbitrary a) => Int -> Gen (TPat a)
-- arbitraryTPat 0 = do˜
--   TPat_Atom Nothing <$> arbitrary
-- arbitraryTPat n
--   | n > 0 = do
--       let n' = n `div` 2
--       oneof
--         [ arbitraryTPatAtom n',
--           arbitraryTPatFast n',
--           arbitraryTPatSlow n'
--           -- Add cases for other constructors here
--         ]
--   | otherwise = arbitraryTPat 0

-- let x = 5

-- >>> QC.sample' genTPat
-- [TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1),TPat_Atom (Nothing) (0 % 1)]

-- quickCheckN :: (Testable prop) => Int -> prop -> IO ()
-- quickCheckN n = QC.quickCheck . QC.withMaxSuccess n

-- prop_timeSigCorrect :: [ArcF Time] -> L.Music -> Bool
-- prop_timeSigCorrect subdivisions music =
--   -- make sure that the smallest common denominator of all the subdivisions
--   -- is the length of the top level time signature
--   let denominators = map (\(Arc start finish) -> denominator start) subdivisions
--       largestCommonDenominator = foldl lcm 1 denominators
--    in case music of
--         Time num den -> num == largestCommonDenominator
--         _ -> False

qc :: IO ()
qc = do
  quickCheck prop

-- quickCheck prop_timeSigCorrect

-- let x = 5
-- >>> qc
