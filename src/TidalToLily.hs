module TidalToLily where

import Control.Applicative ()
import Control.Monad
import Data.Map (Map, empty, lookup)
import Data.Maybe
import Data.Music.Lilypond as L
import Data.Ratio
import Data.Vector qualified as V
import GHC.Generics
import GHC.Real (denominator, numerator)
import Parse
import Sound.Tidal.Pattern as Pattern
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import Text.Parsec (ParseError)
import Text.Pretty

-- import qualified Control.Arrow as them

{-
  Current naive implementation of retrieving the time signature
  from the ControlPattern's subdivisions.
-}
getTimeSig :: [ArcF Time] -> L.Music
getTimeSig subdivisions =
  -- find the smallest common denominator of all the subdivisions
  let denominators = map (\(Arc start finish) -> denominator start) subdivisions
      largestCommonDenominator = foldl lcm 1 denominators
   in Time largestCommonDenominator 8 -- TODO: for now, eighth note is the base unit of time

{-
  Current naive implementation of retrieving the key signature
  from the ControlPattern's subdivisions.
-}
getKeySig :: [Event a] -> Pitch
getKeySig eventLs = Pitch (C, 0, 5)

getNotes :: [Event ValueMap] -> [Music]
getNotes eventLs =
  let pitchMap = getPitchMap $ getKeySig eventLs
   in case getTimeSig $ map (\(Event _ _ arc _) -> arc) eventLs of
        Time num den ->
          concatMap
            (eventToMusic pitchMap num den)
            eventLs
        _ -> error "Time signature not found"

data Unit
  = URest
  | UNote Pitch
  | UChord [Pitch]

eventToMusic :: V.Vector Pitch -> Integer -> Integer -> Event ValueMap -> [Music]
eventToMusic pitchMap num den (Event ctx whole part vm) =
  let (Arc start finish) = part
      duration = finish - start
      duration' = (numerator duration * num) % (denominator duration * den)
      multi = multiNote duration'
   in case parseValueMap pitchMap vm of
        Just URest -> map (\dur -> L.Rest (Just dur) []) multi
        Just (UNote p) -> applySlurs $ map (\dur -> L.Note (NotePitch p Nothing) (Just dur) []) multi
        Just (UChord ps) -> undefined
        Nothing -> error "ValueMap parsing failed"

applySlurs :: [Music] -> [Music]
applySlurs xs@(a : b : r) = [beginSlur $ head xs] ++ tail (init xs) ++ [endSlur $ last xs]
applySlurs xs = xs

-- returns correct lilypond unit with arbitrary duration
parseValueMap :: V.Vector Pitch -> ValueMap -> Maybe Unit
parseValueMap pitchMap vm = msum triedList
  where
    triedList = [UNote <$> getNote pitchMap vm, getRest vm]

-- apply something at the end to modify pitches

-- half-steps for a major scale, with 0 as the base

majorScale :: V.Vector Int
majorScale = V.fromList [0, 2, 4, 5, 7, 9, 11]

invMajorScale :: V.Vector Int
invMajorScale = V.fromList [0, -1, 1, -1, 2, 3, -1, 4, -1, 5, -1, 6]

scaleNeighbors :: V.Vector (Int, Int)
scaleNeighbors = V.fromList [(0, 0), (0, 1), (1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4), (4, 5), (5, 5), (5, 6), (6, 6)]

-- pitch class to preferred accidental (sharp = 1, flat = -1)
pcToAccidental :: V.Vector Int
pcToAccidental = V.fromList [1, 1, 1, -1, 1, 1, 1]

-- get major scale, with accidentals notated canonically
-- octaves of all notes (pre-accidentals) are the same
getMajorScale :: Pitch -> V.Vector Pitch
getMajorScale p =
  V.fromList $
    let (keyClass, accidental, oct) = L.getPitch p
        pcNum = fromEnum keyClass
        x = getModValue p
        prefAccidental = if accidental /= 0 then accidental else pcToAccidental V.! pcNum
        newScale = map (\a -> (a + x) `mod` 12) (V.toList majorScale)
        letters = map (\a -> toEnum $ (a + pcNum) `mod` 7) [0 .. 6]
        initScale = map (\a -> majorScale V.! ((a + pcNum) `mod` 7)) [0 .. 6] -- based only on staff line
        diff = zipWith (\a b -> b - a) initScale newScale
     in if abs accidental > 1
          then error "too many sharps or flats"
          else zipWith (\a b -> Pitch (a, conciseMod $ (b + 12) `mod` 12, 0)) letters diff
  where
    conciseMod x = if x <= 6 then x else x - 12

getModValue :: Pitch -> Int
getModValue (Pitch (a, b, _)) = (majorScale V.! fromEnum a) + b

-- expresses x with the same letter as p, using p's octave
modifyPitch :: Pitch -> Int -> Pitch
modifyPitch p@(Pitch (a, b, c)) x = Pitch (a, b + x - getModValue p, c)

getBestNeighbor :: V.Vector Pitch -> Int -> Int -> Pitch
getBestNeighbor scale pref i =
  let (a, b) = scaleNeighbors V.! i
   in if a == b
        then scale V.! a
        else case invMajorScale V.! noteInt of
          -1 -> modifyPitch (if pref >= 0 then scale V.! a else scale V.! b) noteInt
          x -> Pitch (toEnum x, 0, 0)
  where
    noteInt = (i + getModValue (V.head scale)) `mod` 12

-- given major scale key, returns a length 12 list from note mod values to pitches
-- start from relative, then do rotation
-- imperatively, start with [x, _, x, _ x, x, _, x, _, x, _, x]
getPitchMap :: Pitch -> V.Vector Pitch
getPitchMap p =
  V.fromList $
    let scale = getMajorScale p
        (keyClass, accidental, oct) = L.getPitch p
        pref = if accidental /= 0 then accidental else pcToAccidental V.! fromEnum keyClass
        cDiff = (negate (getModValue p) `mod` 12 + 12) `mod` 12
     in map (getBestNeighbor scale pref . (\a -> (a + cDiff) `mod` 12)) [0 .. 11] -- change 0 to 11 to modular

-- if in scale, then output the same. Otherwise, find something that naturals or follow accidental direction
getPitch2 :: V.Vector Pitch -> Int -> Pitch
getPitch2 pitchMap y =
  let absPitch = ((y `mod` 12) + 12) `mod` 12
      octave = (y - absPitch) `div` 12 + 5
      (a, b, _) = L.getPitch $ pitchMap V.! absPitch
   in Pitch (a, b, octave)

getNote :: V.Vector Pitch -> ValueMap -> Maybe L.Pitch
getNote pitchMap vm = do
  val <- msum [Data.Map.lookup "n" vm, Data.Map.lookup "note" vm]
  return
    ( case val of
        VN note -> getPitch2 pitchMap $ round note
        _ -> error "Note value note found"
    )

getRest :: ValueMap -> Maybe Unit
getRest vm = if null vm then Just URest else Nothing

{-
  Create a slur of notes of power 2 that add up to duration.
  TODO:
  An alternative to creating slurs is to create tuplets. For the future
  use the List monad to generate both slurs and tuplets options as a
  list of possible Music. Then, use a heuristic to choose the best option.
-}
multiNote :: Rational -> [Duration]
multiNote duration =
  let duration' = numerator duration % denominator duration
   in if duration' == 0
        then []
        else
          if duration' == 1
            then [Duration duration]
            else
              let power = floor $ logBase 2 (fromRational duration')
                  dur :: Rational = if power < 0 then 1 % (2 ^ (-1 * power)) else 2 ^ power
                  note = Duration dur
               in note : multiNote (duration - dur)

{-
  A duration is strange if it is not a power of 2 (negative exponents allowed).
-}

insertRests :: [Event ValueMap] -> [Event ValueMap]
insertRests es = foldr f [] (Event {context = Pattern.Context [], whole = Nothing, part = Arc 0 0, value = Data.Map.empty} : es)
  where
    f x ys =
      let Arc _ t1 = part x
          Arc t2 _ = if null ys then Arc 1 1 else part $ head ys
       in if t1 == t2
            then x : ys
            else x : (Event {context = Pattern.Context [], whole = Nothing, part = Arc t1 t2, value = Data.Map.empty}) : ys

-- TODO: somehow find a way to partition into notes and chords
-- given durations, chunk them
-- can look at start and end time events and chunk from there

getClef :: [Event a] -> Music
getClef eventLs = Clef Treble

withinSubset :: ControlPattern -> Bool
withinSubset _ = True -- TODO

controlPatternConverter :: String -> ControlPattern
-- Note, ControlPattern = Pattern ValueMap
controlPatternConverter inputTidalStr =
  let tidalPattern = parseTidal inputTidalStr
   in case tidalPattern of
        Left err -> error $ show err
        Right tidalPat ->
          if withinSubset tidalPat
            then tidalPat
            else error "Tidal pattern not within subset"

-- Create a Composition from multiple rounds of Tidal input/ControlPatterns
type Composition = [Music]

tidalToLilypond :: ControlPattern -> Music
tidalToLilypond tidalPat =
  let eventLs = tail $ insertRests $ queryArc tidalPat (Arc 0 1)
      subdivisions = map part eventLs
   in let timeSig = getTimeSig subdivisions
          clef = getClef eventLs
          notes = getNotes eventLs
       in Sequential [clef, timeSig, Sequential notes]

-- chord interpretation
-- random patterns - use random seed
-- use vector instead of list for scale stuff
