module TidalToLilyCheck where

import Control.Monad (liftM2)
import Data.Music.Lilypond as L
import Data.Ratio
import Sound.Tidal.ParseBP (TPat (..))
import Sound.Tidal.Time (Time (..))
import Test.HUnit
import Test.QuickCheck as QC (Arbitrary (..), Gen, oneof, quickCheck, sample', shrinkNothing, sized, suchThat, withMaxSuccess)
import TidalToLily

instance Arbitrary Time where
  arbitrary = liftM2 (%) QC.arbitrary eight
    where
      eight = return 8
  shrink = QC.shrinkNothing

instance Arbitrary (TPat Rational) where
  arbitrary :: Gen (TPat Rational)
  arbitrary =
    QC.oneof
      [ TPat_Atom <$> noteGen <*> timeGen,
        TPat_Stack <$> QC.arbitrary,
        TPat_Polyrhythm <$> QC.arbitrary <*> QC.arbitrary,
        TPat_Repeat <$> QC.arbitrary <*> QC.arbitrary
        -- TPat_Silence <$ QC.arbitrary
        -- TPat_Fast <$> QC.arbitrary,
        -- TPat_Slow
        --   <$> QC.arbitrary,
        -- TPat_Seq
        --   <$> QC.arbitrary
        --   <*> QC.arbitrary
      ]
    where
      -- generate rationals with denominator 8
      timeGen :: Gen Rational = liftM2 (%) (QC.arbitrary `suchThat` (> 0)) (return 8)
      -- generate Maybe ((Int, Int), (Int, Int)) where ints are positive
      noteGen = do
        num1 :: Int <- QC.arbitrary `suchThat` (> 0)
        den1 :: Int <- QC.arbitrary `suchThat` (> 0)
        num2 :: Int <- QC.arbitrary `suchThat` (> 0)
        den2 :: Int <- QC.arbitrary `suchThat` (> 0)
        return (Just ((num1, den1), (num2, den2)))
  shrink tpat = case tpat of
    TPat_Atom Nothing _ -> []
    TPat_Atom (Just ((num1, den1), (num2, den2))) t ->
      [TPat_Atom Nothing t]
        ++ [TPat_Atom (Just ((num1', den1), (num2, den2))) t | num1' <- QC.shrink num1]
        ++ [TPat_Atom (Just ((num1, den1'), (num2, den2))) t | den1' <- QC.shrink den1]
        ++ [TPat_Atom (Just ((num1, den1), (num2', den2))) t | num2' <- QC.shrink num2]
        ++ [TPat_Atom (Just ((num1, den1), (num2, den2'))) t | den2' <- QC.shrink den2]
    TPat_Stack t -> [TPat_Stack t]
    TPat_Polyrhythm t1 t2 -> [TPat_Polyrhythm t1' t2' | t1' <- QC.shrink t1, t2' <- QC.shrink t2]
    _ -> [TPat_Silence]

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
