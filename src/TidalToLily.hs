module TidalToLily where

import Control.Applicative ()
import Data.Map (Map, lookup)
import Data.Maybe
import Data.Music.Lilypond as L
import Data.Ratio
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
getKeySig :: [Event a] -> Music
getKeySig eventLs = Key (Pitch (C, 0, 5)) Major

getNotes :: [Event a] -> [Music]
getNotes eventLs =
  case getTimeSig $ map (\(Event _ _ arc _) -> arc) eventLs of
    Time num den ->
      concatMap
        (eventToMusic num den)
        eventLs
    _ -> error "Time signature not found"

eventToMusic :: Integer -> Integer -> Event a -> [Music]
eventToMusic num den (Event ctx whole part val) =
  let (Arc start finish) = part
      duration = finish - start
      duration' = (numerator duration * num) % (denominator duration * den)
   in if isStrange duration'
        then
          let firstNote = beginSlur $ head $ multiNote duration'
              lastNote = endSlur $ last $ multiNote duration'
           in [firstNote] ++ tail (init $ multiNote duration') ++ [lastNote]
        else [L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration duration')) []]

-- apply something at the end to modify pitches

-- half-steps for a major scale, with 0 as the base
majorScale :: [Int]
majorScale = [0, 2, 4, 5, 7, 9, 11]

invMajorScale :: [Int]
invMajorScale = [0, -1, 1, -1, 2, 3, -1, 4, -1, 5, -1, 6]

scaleNeighbors :: [[Int]]
scaleNeighbors = [[0], [0, 1], [1], [1, 2], [2], [3], [3, 4], [4], [4, 5], [5], [5, 6], [6]]

-- pitch class to preferred accidental (sharp = 1, flat = -1)
pcToAccidental :: [Int]
pcToAccidental = [1, 1, 1, -1, 1, 1, 1]

-- get major scale, with accidentals notated canonically
-- octaves of all notes (pre-accidentals) are the same
getMajorScale :: Pitch -> [Pitch]
getMajorScale p =
  let (keyClass, accidental, oct) = L.getPitch p
      pcNum = fromEnum keyClass
      x = getModValue p
      prefAccidental = if accidental /= 0 then accidental else pcToAccidental !! pcNum
      newScale = map (\a -> (a + x) `mod` 12) majorScale
      letters = map (\a -> toEnum $ (a + pcNum) `mod` 7) [0 .. 6]
      initScale = map (\a -> majorScale !! ((a + pcNum) `mod` 7)) [0 .. 6] -- based only on staff line
      diff = zipWith (\a b -> b - a) initScale newScale
   in if abs accidental > 1
        then error "too many sharps or flats"
        else zipWith (\a b -> Pitch (a, conciseMod $ (b + 12) `mod` 12, 0)) letters diff
  where
    conciseMod x = if x <= 6 then x else x - 12

getModValue :: Pitch -> Int
getModValue (Pitch (a, b, _)) = (majorScale !! fromEnum a) + b

-- expresses x with the same letter as p, using p's octave
modifyPitch :: Pitch -> Int -> Pitch
modifyPitch p@(Pitch (a, b, c)) x = Pitch (a, b + x - getModValue p, c)

getBestNeighbor :: [Pitch] -> Int -> Int -> Pitch
getBestNeighbor scale pref i = case scaleNeighbors !! i of
  [a] -> scale !! a
  -- add thing that handles naturals
  [a, b] -> case invMajorScale !! noteInt of
    -1 -> modifyPitch (if pref >= 0 then scale !! a else scale !! b) noteInt
    x -> Pitch (toEnum x, 0, 0)
    where
      noteInt = (i + getModValue (head scale)) `mod` 12
  _ -> error "invalid scaleNeighbors"

-- given major scale key, returns a length 12 list from note mod values to pitches
-- start from relative, then do rotation
-- imperatively, start with [x, _, x, _ x, x, _, x, _, x, _, x]
getPitchMap :: Pitch -> [Pitch]
getPitchMap p =
  let scale = getMajorScale p
      (keyClass, accidental, oct) = L.getPitch p
      pref = if accidental /= 0 then accidental else pcToAccidental !! fromEnum keyClass
      cDiff = (negate (getModValue p) `mod` 12 + 12) `mod` 12
   in map (getBestNeighbor scale pref . (\a -> (a + cDiff) `mod` 12)) [0 .. 11] -- change 0 to 11 to modular

-- if in scale, then output the same. Otherwise, find something that naturals or follow accidental direction
getPitch2 :: [Pitch] -> Int -> Pitch
getPitch2 pitchMap y =
  let absPitch = ((y `mod` 12) + 12) `mod` 12
      octave = (y - absPitch) `div` 12 + 5
      (a, b, _) = L.getPitch $ pitchMap !! absPitch
   in Pitch (a, b, octave)

-- map (\a -> majorScale !! (a + pcNum) % 8) [0 .. 7]

-- rule for getting accidental
-- if occurs on major scale, then do whatever that note is
-- otherwise, find nearest guy brought up or down
-- getPitch :: Pitch -> Int -> Pitch -- key, note int
-- getPitch p y =
--   let (keyClass, accidental, oct) = L.getPitch p
--       pcNum = fromEnum keyClass
--       pitch = y `mod` 12
--       octave = y `div` 12 + 5
--       prefAccidental = if accidental != 0 then accidental else pcToAccidental !! pcNum
--    in if (abs accidental) > 1
--         then error "too many sharps or flats"
--         else undefined

-- let (keyClass, accidental, oct) = L.getPitch p
--     enumKey = fromEnum keyClass
--     x = (majorScale !! enumKey) + accidental
--     pitch = y `mod` 12
--     octave = y `div` 12 + 5
--     scale = zipWith (\i a -> (enumKey + i `mod` 8, (x + a) `mod` 12)) [0 .. 7] majorScale
--  in undefined

getNote :: [Pitch] -> ValueMap -> Maybe L.Pitch
getNote pitchMap vm = do
  val <- Data.Map.lookup "n" vm
  return
    ( case val of
        VN note -> getPitch2 pitchMap $ round note
        _ -> error "Note value note found"
    )

-- case Data.Map.lookup "n" vm of
--   Just val -> case val of
--     VN note -> Just $ getPitch note where
--       getPitch
--     _ -> error "Note value not found"
--   Nothing -> Nothing

{-
  Create a slur of notes of power 2 that add up to duration. Similar
  to the DP coin change problem.
  TODO:
  An alternative to creating slurs is to create tuplets. For the future
  use the List monad to generate both slurs and tuplets options as a
  list of possible Music. Then, use a heuristic to choose the best option.
-}
multiNote :: Rational -> [Music]
multiNote duration =
  let duration' = numerator duration % denominator duration
   in if duration' == 0
        then []
        else
          if duration' == 1
            then [L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration duration)) []]
            else
              let power = floor $ logBase 2 (fromRational duration')
                  dur :: Rational = if power < 0 then 1 % (2 ^ (-1 * power)) else 2 ^ power
                  note = L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration dur)) []
               in note : multiNote (duration - dur)

{-
  A duration is strange if it is not a power of 2 (negative exponents allowed).
-}
isStrange :: Rational -> Bool
isStrange duration =
  let n = numerator duration
      d = denominator duration
   in if n == 1
        then (d /= 1) && (odd d || isStrange (n % (d `div` 2)))
        else odd n || isStrange ((n `div` 2) % d)

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
  let eventLs = queryArc tidalPat (Arc 0 1)
      subdivisions = map part eventLs
   in let timeSig = getTimeSig subdivisions
          clef = getClef eventLs
          notes = getNotes eventLs
       in Sequential [clef, timeSig, Sequential notes]