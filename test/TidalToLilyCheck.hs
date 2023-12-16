module TidalToLilyCheck where

import Test.HUnit
import Test.QuickCheck
import Data.Music.Lilypond as L
import TidalToLily (tidalToLilypond)

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