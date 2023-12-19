module TidalToLilyCheck where

import Control.Monad (liftM2)
import Data.Ratio
import Sound.Tidal.ParseBP (TPat (..), tShow)
import Sound.Tidal.Time (Time (..))
import Test.HUnit
import Test.QuickCheck as QC (Arbitrary (..), Gen, elements,
 oneof, quickCheck, vectorOf, sample',sized, shrinkNothing,
 sized, suchThat, withMaxSuccess, vectorOf, Property)
import TidalToLily
import LilypondPlus.LilypondPlus as LP
import qualified Data.Sequence as LP
import Sound.Tidal.Pattern as Pattern (ControlPattern, queryArc,
  ArcF (Arc), Event, ValueMap, EventF (..))
import Parse
import System.IO
import System.Process (callCommand)
import Text.Pretty
import Test.QuickCheck.Monadic (assert, monadicIO, run, pre)
import TranslationStepper2

newtype TidalString = TidalString String deriving (Eq, Show)

instance Arbitrary TidalString where
  arbitrary = do
    tidalStr <- tPP <$> (QC.arbitrary :: Gen (TPat Rational))
    let contextTidalStr = "n \"" ++ tidalStr ++ "\""
    return $ TidalString contextTidalStr
  shrink = QC.shrinkNothing

instance Arbitrary Time where
  arbitrary = liftM2 (%) QC.arbitrary eight
    where
      eight = return 8
  shrink = QC.shrinkNothing

instance Arbitrary (TPat Rational) where
  arbitrary :: Gen (TPat Rational)
  arbitrary = sized arbitraryTPat
  shrink tpat = case tpat of
    TPat_Atom Nothing _ -> []
    TPat_Atom (Just ((num1, den1), (num2, den2))) t ->
      [TPat_Atom Nothing t]
        ++ [TPat_Atom (Just ((num1', den1), (num2, den2))) t |
              num1' <- QC.shrink num1]
        ++ [TPat_Atom (Just ((num1, den1'), (num2, den2))) t |
              den1' <- QC.shrink den1]
        ++ [TPat_Atom (Just ((num1, den1), (num2', den2))) t |
              num2' <- QC.shrink num2]
        ++ [TPat_Atom (Just ((num1, den1), (num2, den2'))) t |
              den2' <- QC.shrink den2]
    TPat_Stack t -> [TPat_Stack t]
    TPat_Polyrhythm t1 t2 -> [TPat_Polyrhythm t1' t2' |
      t1' <- QC.shrink t1, t2' <- QC.shrink t2]
    _ -> [TPat_Silence]


tPP :: TPat Rational -> String
tPP tpat = case tpat of
  TPat_Atom _ t -> mapToKey t
  TPat_Seq t -> concatMap tPP t
  TPat_Silence -> " ~ "
  -- TPat_Elongate n t -> tPP t ++ replicate (fromInteger (div (numerator n) (denominator n) :: Integer)) '_'
  _ -> error "tPP: not implemented"

mapToKey :: Rational -> String
mapToKey k = 
  let approxOctave = div ((numerator k `div` denominator k :: Integer) + 88 ) 12 in 
  (case (numerator k `div` denominator k :: Integer) `mod` 12 of
    0 -> "c"
    1 -> "cs"
    2 -> "d"
    3 -> "ds"
    4 -> "e"
    5 -> "f"
    6 -> "fs"
    7 -> "g"
    8 -> "gs"
    9 -> "a"
    10 -> "as"
    11 -> "b"
    _ -> error "impossible") ++ show approxOctave

-- Arbitrary generates a list of TPat Rational, comprising only of 
  -- TPat_Atoms or TPat_Silence

arbitraryTPatList :: Gen [TPat Rational]
arbitraryTPatList = do
  n <- QC.arbitrary `suchThat` (\x -> x > 0 && x <= 50)
  QC.vectorOf n arbitraryTPatLeaves

arbitraryTPatLeaves :: Gen (TPat Rational)
arbitraryTPatLeaves = do
  QC.oneof [
    TPat_Atom <$> locGen <*> keyGen,
    TPat_Elongate <$> QC.arbitrary `suchThat` (\x -> denominator x == 1 && x > 0) <*> arbitraryTPatLeaves,
    return TPat_Silence
    ]


arbitraryTPat :: Int -> Gen (TPat Rational)
arbitraryTPat 0 = do
   TPat_Atom <$> locGen <*> keyGen
arbitraryTPat n = do
    QC.oneof
      [ TPat_Atom <$> locGen <*> keyGen,
        -- TPat_Stack <$> QC.arbitrary,
        -- TPat_Polyrhythm <$> QC.arbitrary <*> QC.arbitrary,
        -- TPat_Repeat <$> QC.arbitrary <*> QC.arbitrary,
        -- return TPat_Silence,
        -- TPat_Fast <$> QC.arbitrary,
        -- TPat_Slow
        --   <$> QC.arbitrary,
        TPat_Seq <$> arbitraryTPatList
      ]


keyGen :: Gen Rational
keyGen = liftM2 (%) (QC.elements [-51..36]) (return 1)
-- (QC.arbitrary `suchThat` (\x -> x > -60 && x < 44))

-- Generate Maybe ((Int, Int), (Int, Int)) where ints are positive
locGen = do
  num1 :: Int <- QC.arbitrary `suchThat` (> 0)
  den1 :: Int <- QC.arbitrary `suchThat` (> 0)
  num2 :: Int <- QC.arbitrary `suchThat` (> 0)
  den2 :: Int <- QC.arbitrary `suchThat` (> 0)
  QC.oneof [
    return Nothing,
    return $ Just ((num1, den1), (num2, den2))
    ]


{- 
  QuickCheck property for round trip testing from Tidal -> Lilypond -> MIDI
  -> Tidal. Takes a Tidal pattern (arbitrarily generated) and converts it to
-}
roundTripProp :: TidalString -> Property
roundTripProp (TidalString tidalStr) =
  monadicIO $ do
    pre $ notSilentOnly tidalStr
    -- Convert to Lilypond
    let
      controlPattern = case parseTidal tidalStr of
        Left _ -> error "Could not parse Tidal pattern"
        Right cp -> cp
      translation = translateOneCycle (0 :: Time) tidalStr
    -- Convert back to Tidal using LilypondToTidal
    tidalCmd <- run $ lilypondToTidal "debug/test.ly" (fromJust translation)
    return $ controlPatternsEqual tidalCmd controlPattern

notSilentOnly :: String -> Bool
notSilentOnly tidalStr = case parseTidal tidalStr of
  Left _ -> error "Could not parse Tidal pattern"
  Right cp -> case queryArc cp (Arc 0 1) of
    [] -> False
    _ -> True

controlPatternsEqual :: ControlPattern -> ControlPattern -> Bool
controlPatternsEqual cp1 cp2 =
  let cp1EVMap = queryArc cp1 (Arc 0 1)
      cp2EVMap = queryArc cp2 (Arc 0 1)
  in
    aux cp1EVMap cp2EVMap
    where
    aux :: [Event ValueMap] -> [Event ValueMap] -> Bool
    aux ev1 ev2 =
      -- There is no Eq instance for Event ValueMap, so we have to do this
      case (ev1, ev2) of
        ([], []) -> True
        ([], _) -> False
        (_, []) -> False
        (x:xs, y:ys) ->
          value x == value y &&
          whole x == whole y &&
          part x == part y &&
          aux xs ys


fromJust :: Maybe Music -> Music
fromJust (Just m) = m
fromJust Nothing = error "fromJust: Nothing"


--------------------------------------------------------------------------------
-- | Utilities for round trip testing (Tidal -> Lilypond -> MIDI -> Tidal)

lilypondToTidal :: String -> Music -> IO ControlPattern
lilypondToTidal fileName convertedLilypondAST = do
  -- Writes the lilypond file
  handle <- openFile fileName WriteMode
  let lilypondFileAST = File [Version "2.24.3", Score convertedLilypondAST]
  hPutStr handle $ show $ pretty lilypondFileAST
  hClose handle
  -- Converts to actual score as pdf
  callCommand "lilypond -dmidi-extension=mid --output=debug/ debug/test.ly"
  callCommand ("python midi_to_tidalcycles/src/midi_to_tidalcycles.py " ++
    "--shape " ++
    "--resolution=2 " ++
    "debug/test.mid > debug/test.tidal")
  -- Get the string from the file
  handle' <- openFile "debug/test.tidal" ReadMode
  contents <- hGetContents handle'
  putStrLn contents
  tidalCmd <- getTidal contents
  print tidalCmd
  hClose handle'
  return tidalCmd


getTidal :: String -> IO ControlPattern
getTidal s = case controlPatternConverter $ extractSound s of
  Just cp -> return cp
  Nothing -> error "Could not parse Tidal pattern"
  where
    -- drop everything before and including first $
    extractSound :: String -> String
    extractSound s = case dropWhile (/= '$') s of
      [] -> []
      _:s' -> s'


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


-- let x = QC.sample' (arbitrary :: Gen (TPat Rational))
--     func i = x >>= (\a -> return (a !! i) ) >>= print



qc :: IO ()
qc = do
  quickCheck roundTripProp
  -- quickCheckN 1000 prop_timeSigCorrect


-- | Lilypond Arbitrary AST Generation

-- instance Arbitrary LP.PitchClass where
--   arbitrary = QC.elements [LP.C, LP.D, LP.E, LP.F, LP.G, LP.A, LP.B]
--   shrink = QC.shrinkNothing

-- instance Arbitrary LP.Note where
--   arbitrary = return (LP.NotePitch (LP.Pitch (LP.C, 0, 5)) Nothing)
--   shrink = QC.shrinkNothing

-- instance Arbitrary LP.Duration where
--   arbitrary = return LP.Duration {LP.getDuration = 1 % 4}
--   shrink = QC.shrinkNothing

-- arbitraryUnit :: Gen LP.Music
-- arbitraryUnit = QC.oneof [
--       LP.Time <$> QC.arbitrary `suchThat` (\x -> x <= 20 && x > 0) <*> return 8,
--       (LP.Rest . Just <$> QC.arbitrary) <*> return [],
--       LP.Note <$> QC.arbitrary <*> QC.arbitrary <*> return []
--     ]

-- arbitrarySequential :: Int -> Gen [LP.Music]
-- arbitrarySequential 0 = return []
-- arbitrarySequential n = do
--   -- let n' = n `div` 2
--   -- LP.Sequential [] where the list contains LP.Time, LP.Rest, LP.Note, as above
--   QC.oneof [
--     arbitraryUnit >>= \x -> return [x],
--     arbitrarySequential n
--     ]


-- instance Arbitrary LP.Music where
--   arbitrary = LP.Sequential <$> sized arbitrarySequential
--   shrink = aux where
--     aux :: LP.Music -> [LP.Music]
--     aux (LP.Sequential (x:xs)) = LP.Sequential xs : aux x
--     aux _ = []
