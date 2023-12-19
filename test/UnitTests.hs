module UnitTests
where

import Test.HUnit
import TidalToLily
import Sound.Tidal.Context
import Data.Vector qualified as V
import LilypondPlus.LilypondPlus
import Test.QuickCheck (Gen, Property,
   Arbitrary(..), Testable(..),
   (==>))
import qualified Test.QuickCheck as QC
import Sound.Tidal.Pattern as Pattern
import qualified Data.Functor
import qualified Data.Map as Map
import ParseConversionCmd

testGetTimeSig :: Test
testGetTimeSig = "getTimeSig" ~:
    TestList [
        getTimeSig [Arc 0 1] ~?= (1, 8),
        getTimeSig [Arc (1 % 2) (5 % 8)] ~?= (8, 8),
        getTimeSig [Arc (1 % 2) (5 % 8), Arc (8000 % 9000) (15 % 16)]
            ~?= (144, 8)
    ]

testCountNumInScale :: Test
testCountNumInScale = "countNumInScale" ~:
    TestList [
        flip countNumInScale [0 .. 11] <$> [0 .. 11] ~?= replicate 12 13,
        countNumInScale (negate 1) [11, 14, 18] ~?= 6,
        countNumInScale 2 [11, 14, 18] ~?= 9
    ]

testGetBestFitPitch :: Test
testGetBestFitPitch = "getBestFitPitch" ~:
    TestList [
        -- Mary had a little lamb in d-flat major
        getBestFitPitch [5, 3, 1, 3, 5, 5, 5, 3, 3, 3, 5, 8, 8]
            ~?= Pitch (D, -1, 0),
        -- d-flat minor version of same song
        getBestFitPitch [4, 3, 1, 3, 4, 4, 4, 3, 3, 3, 4, 8, 8]
            ~?= Pitch (E, 0, 0)
    ]

-- test pitchMap generation

pitchesToTest = [Pitch (letter, acc, oct) | letter <- [C, D, E, F, G, A, B],
    acc <- [-1, 0, 1], oct <- [(negate 5) .. 5]]
noteIntsToTest = [(negate 100) .. 100]

testGetPitchMap :: Test
testGetPitchMap = "getPitchMap" ~:
    TestList [
        V.toList (getRelValue <$> getPitchMap p) == [0 .. 11] ~?= True
        | p <- pitchesToTest
    ]

testGetPitch :: Test
testGetPitch = "getPitch" ~:
    TestList [
        (+ negate 60) . getRelValue . getPitch2 (getPitchMap p)
            <$> noteIntsToTest ~?= noteIntsToTest
    | p <- pitchesToTest]

fromDuration :: Duration -> Rational
fromDuration (Duration dur) = dur

testMultiNote :: Test
testMultiNote = "multiNote" ~:
    TestList [
        sum (fromDuration <$> multiNote (a % b)) ~?= a % b
        | p <- [0..6], let b = 2^p, a <- [0..b]
    ]

-- TODO: test insertRests, getFullRep <$> getChordRep

testInsertRests :: Test
testInsertRests = "insertRests" ~:
    TestList [
        length (insertRests [mkEvent 0 1 []]) ~?= 2,
        length (insertRests [mkEvent 0 (1 % 2) []]) ~?= 3,
        length (insertRests [mkEvent 0 (1 % 2) [], mkEvent (3 % 4) (7 % 8) []]) ~?= 5
    ]

mkEvent :: Rational -> Rational -> a -> Event a
mkEvent st sp filler = Event {context = Pattern.Context [], whole = Just $ Arc st sp, part = Arc st sp, value = filler}

testGetFullAndChord :: Test
testGetFullAndChord = "getFullRep and getChordRep" ~:
    TestList [
        composed [mkEvent 0 (2 % 3) Map.empty, mkEvent (1 % 3) 1 Map.empty] ~?= Nothing,
        composed [mkEvent (1 % 3) (2 % 3) Map.empty, mkEvent (1 % 4) (3 % 4) Map.empty] ~?= Nothing,
        composed [mkEvent (1 % 3) (1 % 2) Map.empty, mkEvent (1 % 4) (3 % 4) Map.empty] ~?= Nothing,
        (composed [mkEvent 0 1 Map.empty, mkEvent 0 1 Map.empty] Data.Functor.<&> length) ~?= Just 1,
        (composed [mkEvent 0 (1 % 2) Map.empty, mkEvent (1 % 2) 1 Map.empty, mkEvent (1 % 2) 1 Map.empty] 
            Data.Functor.<&> length) ~?= Just 2
    ]
    where
        composed x = getChordRep x Data.Functor.<&> getFullRep



-- -- | Parsing tests for the stepper 
-- testParseCommands :: Test
-- testParseCommands = "test parsing commands for stepper" ~:
--     TestList [
--         parse setOptionParser "" "advance     2" ~?= Right (Advance 2),
--         show . parse setOptionParser "" "d2    $    s    \"bd\"" ~?= "Right (CP 2 (0>1)|s: \"bd\")",
--         show . parse setOptionParser "" "d2 $ s \"[bd bd bd] bd bd\"" ~?= "Right (CP 2  (0>⅑)|s: \"bd\" (⅑>²₉)|s: \"bd\" (²₉>⅓)|s: \"bd\" (⅓>⅔)|s: \"bd\" (⅔>1)|s: \"bd\")"
--     ]
