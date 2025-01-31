{-# LANGUAGE TupleSections #-}

module TidalToLily
where

import Control.Applicative ()
import Control.Monad
import Data.Char
import Data.Map (Map, empty, fromList, insert, lookup, member, toAscList)
import Data.Maybe
import Data.Ratio
import Data.Set (Set, fromList, member)
import Data.Vector qualified as V
import GHC.Generics
import GHC.Real (denominator, numerator)
import LilypondPlus.LilypondPlus as L
  ( Clef (Percussion, Treble),
    DrumPitch (..),
    Duration (Duration),
    Mode (Major),
    Music (Chord, Clef, Key, Rest, Sequential, Time, Drums),
    Note (DrumNotePitch, NotePitch),
    Pitch (..),
    PitchClass (..),
    beginTie,
  )
import Parse
import Sound.Tidal.Context (bandf, n, xDefault)
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
import qualified Data.Bifunctor
import qualified Data.Functor as Data.Bifunctor

-- import qualified Control.Arrow as them

{-
  Current eighth note base implementation of retrieving the time signature
  from the ControlPattern's subdivisions.
-}
getTimeSig :: [ArcF Time] -> (Integer, Integer)
getTimeSig subdivisions =
  -- find the smallest common denominator of all the subdivisions
  let denominators = map 
            (\(Arc start finish) -> lcm (denominator start) 
              (denominator finish)) 
            subdivisions
      largestCommonDenominator = foldl lcm 1 denominators
   in (largestCommonDenominator, 8)

{-
  Naive implementation of retrieving the key signature
  from the ControlPattern's subdivisions.
-}

-- first make sure everything has the n feature. then, make a count of
getKeySig :: [Event ValueMap] -> Maybe Pitch
getKeySig [] = Just $ Pitch (C, 0, 0)
getKeySig eventLs = do
  ps <- collectPitches eventLs
  return $ getBestFitPitch ps

collectPitches :: [Event ValueMap] -> Maybe [Int]
collectPitches = mapM (\e -> 
    msum (toNoteInt . flip Data.Map.lookup (value e) <$> ["n", "note"]))
  where
    toNoteInt (Just (VN n)) = Just $ round n
    toNoteInt _ = Nothing

countNumInScale :: Int -> [Int] -> Int
countNumInScale shift notes = cnt majorScaleSet notes + 
  max (2 * cnt majorDomSet notes) (2 * cnt minorDomSet notes)
  where
    cnt set = length . filter (\x -> Data.Set.member ((x - shift) `mod` 12) set)

getBestFitPitch :: [Int] -> Pitch
getBestFitPitch notes = keySigs V.! snd (foldr compUpd (-1, -1) [0 .. 11])
  where
    compUpd s tup =
      let res = countNumInScale s notes
       in max (res, s) tup

-- After collecting pitches, 
-- iterate through major scales and see which one hits most notes

getNotes :: Maybe (V.Vector Pitch) -> [Event [ValueMap]] -> Maybe [Music]
getNotes mPitchMap eventLs = 
  case getTimeSig $ map (\(Event _ _ arc _) -> arc) eventLs of
  (num, den) -> foldr (f . eventToMusic mPitchMap num den) (Just []) eventLs
    where
      f x b = do
        b' <- b
        x' <- x
        return $ x' ++ b'

data Unit
  = URest
  | UChord [L.Note]
  deriving (Show)

eventToMusic :: Maybe (V.Vector Pitch) -> Integer -> Integer ->
  Event [ValueMap] -> Maybe [Music]
eventToMusic mPitchMap num den (Event ctx whole part vms) =
  let (Arc start finish) = part
      duration = finish - start
      duration' = (numerator duration * num) % (denominator duration * den)
      multi = multiNote duration'
   in case parseValueMap mPitchMap vms of
        Just URest -> Just $ map (\dur -> L.Rest (Just dur) []) multi
        Just (UChord ps) -> Just $ applyTies $ map
          (\dur -> L.Chord (map (,[]) ps) (Just dur) []) multi
        Nothing -> Nothing

applyTies :: [Music] -> [Music]
applyTies xs@(a : b : r) = (beginTie <$> init xs) ++ [last xs]
applyTies xs = xs

-- Returns correct lilypond unit with arbitrary duration
parseValueMap :: Maybe (V.Vector Pitch) -> [ValueMap] -> Maybe Unit
parseValueMap mPitchMap vms = msum triedList
  where
    triedList = [getRest vms, UChord <$> mapM (getNote mPitchMap) vms]

-- Half-steps for a major scale, with 0 as the base

majorScale :: V.Vector Int
majorScale = V.fromList [0, 2, 4, 5, 7, 9, 11]

majorScaleSet :: Set Int
majorScaleSet = Data.Set.fromList [0, 2, 4, 5, 7, 9, 11]

majorDomSet :: Set Int
majorDomSet = Data.Set.fromList [0, 4, 7]

minorDomSet :: Set Int
minorDomSet = Data.Set.fromList [9, 0, 4]

keySigs :: V.Vector Pitch
keySigs = V.fromList
  $ Pitch
  <$>
  [(C, 0, 0), (D, -1, 0), (D, 0, 0), (E, -1, 0), (E, 0, 0), (F, 0, 0),
  (G, -1, 0), (G, 0, 0), (A, -1, 0), (A, 0, 0), (B, -1, 0), (B, 0, 0)]

invMajorScale :: V.Vector Int
invMajorScale = V.fromList [0, -1, 1, -1, 2, 3, -1, 4, -1, 5, -1, 6]

scaleNeighbors :: V.Vector (Int, Int)
scaleNeighbors = V.fromList [(0, 0), (0, 1), (1, 1), (1, 2), (2, 2), (3, 3),
  (3, 4), (4, 4), (4, 5), (5, 5), (5, 6), (6, 6)]

-- Pitch class to preferred accidental (sharp = 1, flat = -1)
pcToAccidental :: V.Vector Int
pcToAccidental = V.fromList [1, 1, 1, -1, 1, 1, 1]

-- Get major scale, with accidentals notated canonically
-- octaves of all notes (pre-accidentals) are the same
getMajorScale :: Pitch -> V.Vector Pitch
getMajorScale p =
  V.fromList $
    let (keyClass, accidental, oct) = L.getPitch p
        pcNum = fromEnum keyClass
        x = getRelValue p
        prefAccidental = if accidental /= 0
          then accidental else pcToAccidental V.! pcNum
        newScale = map (\a -> (a + x) `mod` 12) (V.toList majorScale)
        letters = map (\a -> toEnum $ (a + pcNum) `mod` 7) [0 .. 6]
        initScale = map (\a -> majorScale V.! ((a + pcNum) `mod` 7)) [0 .. 6]
        diff = zipWith (flip (-)) initScale newScale
     in if abs accidental > 1
          then error "too many sharps or flats"
          else zipWith (\a b -> handleWeirdOctave $
          Pitch (a, conciseMod $ b `mod` 12, 0)) letters diff
  where
    conciseMod x = if x <= 6 then x else x - 12
    handleWeirdOctave op@(Pitch (x, y, z)) =
      let rv = getRelValue op
       in Pitch (x, y, if rv >= 12 then -1 else if rv < 0 then 1 else 0)

getRelValue :: Pitch -> Int
getRelValue (Pitch (a, b, c)) = (majorScale V.! fromEnum a) + b + 12 * c

-- Expresses x with the same letter as p, using p's octave
modifyPitch :: Pitch -> Int -> Pitch
modifyPitch p@(Pitch (a, b, c)) x = Pitch (a, b + x - getRelValue p, c)

getBestNeighbor :: V.Vector Pitch -> Int -> Int -> Pitch
getBestNeighbor scale pref i =
  let (a, b) = scaleNeighbors V.! i
   in if a == b
        then scale V.! a
        else case invMajorScale V.! noteInt of
          -1 -> modifyPitch
            (if pref >= 0 then scale V.! a else scale V.! b) noteInt
          x -> Pitch (toEnum x, 0, 0)
  where
    noteInt = (i + getRelValue (V.head scale)) `mod` 12

-- Given major scale key, 
-- returns a length 12 list from note mod values to pitches
-- start from relative, then do rotation
-- imperatively, start with [x, _, x, _ x, x, _, x, _, x, _, x]
getPitchMap :: Pitch -> V.Vector Pitch
getPitchMap p =
  V.fromList $
    let scale = getMajorScale p
        (keyClass, accidental, oct) = L.getPitch p
        pref = if accidental /= 0
          then accidental else pcToAccidental V.! fromEnum keyClass
        cDiff = (negate (getRelValue p) `mod` 12) `mod` 12
     in
      map (getBestNeighbor scale pref . (\a -> (a + cDiff) `mod` 12)) [0 .. 11]

-- If in scale, then output the same. 
-- Otherwise, find something that naturals or follow accidental direction
getPitch2 :: V.Vector Pitch -> Int -> Pitch
getPitch2 pitchMap y =
  let absPitch = y `mod` 12
      octave = (y - absPitch) `div` 12 + 5
      (a, b, o) = L.getPitch $ pitchMap V.! absPitch
   in Pitch (a, b, o + octave)

getNote :: Maybe (V.Vector Pitch) -> ValueMap -> Maybe L.Note
getNote (Just pitchMap) vm = do
  val <- msum (flip Data.Map.lookup vm <$> ["n", "note"])
  case val of
    VN note -> return $ NotePitch (getPitch2 pitchMap $ round note) Nothing
    _ -> Nothing
getNote Nothing vm = do
  val <- msum (flip Data.Map.lookup vm <$> ["s", "sound"])
  case val of
    VS str -> flip DrumNotePitch Nothing <$> Data.Map.lookup str drumPitches
    _ -> Nothing

drumPitches :: Data.Map.Map String L.DrumPitch
drumPitches = Data.Map.fromList (map (\i ->
  (toLower <$> show (toEnum i :: L.DrumPitch), toEnum i)) [0 .. 18])

getRest :: [ValueMap] -> Maybe Unit
getRest vms = if null vms then Just URest else Nothing

{-
  Create a tie of notes of power 2 that add up to duration.
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
                  dur :: Rational = if power < 0
                    then 1 % (2 ^ (-1 * power)) else 2 ^ power
                  note = Duration dur
               in note : multiNote (duration - dur)

cycleRest :: Event [ValueMap]
cycleRest = Event {context = Pattern.Context [], whole = Just (Arc 0 1),
part = Arc 0 1, value = []}

insertRests :: [Event [ValueMap]] -> [Event [ValueMap]]
insertRests es = foldr f [] (Event {context = Pattern.Context [],
whole = Nothing, part = Arc 0 0, value = []} : es)
  where
    f x ys =
      let Arc _ t1 = part x
          Arc t2 _ = if null ys then Arc 1 1 else part $ head ys
       in if t1 == t2
            then x : ys
            else x : (Event {context = Pattern.Context [], whole = Nothing,
            part = Arc t1 t2, value = []}) : ys

-- Reject control sequences where 2 overlapping events don't overlap perfectly

getChordRep :: [Event ValueMap] -> Maybe (Map Time (Time, [ValueMap]))
getChordRep vms = do 
  midMap <- (foldr handleElem $ Just Data.Map.empty) vms
  checkOverlaps midMap
    where
      checkOverlaps tmap = let
        flattened = toAscList tmap 
        flatList = foldr (\(a, t) b -> a : fst t : b) [] flattened
        in
        if isSorted flatList then
          Just tmap
        else
          Nothing

-- from Data.List.Ordered
isSorted :: Ord a => [a] -> Bool
isSorted (x : y : list) = (x <= y) && isSorted (y : list)
isSorted _ = True

handleElem :: Event ValueMap -> Maybe (Map Time (Time, [ValueMap])) ->
  Maybe (Map Time (Time, [ValueMap]))
handleElem _ Nothing = Nothing
handleElem e (Just m) =
  let (st, en, vm) = (start $ part e, stop $ part e, value e)
      m' = if Data.Map.member st m then m else Data.Map.insert st (en, []) m
   in do
        x <- Data.Map.lookup st m'
        if fst x == en
          then return $ Data.Map.insert st (Data.Bifunctor.second (vm :) x) m'
          else Nothing

getFullRep :: Map Time (Time, [ValueMap]) -> [Event [ValueMap]]
getFullRep m = (\(t0, (t1, vm)) -> Event {context = Pattern.Context [],
whole = Nothing, part = Arc t0 t1, value = vm}) <$> Data.Map.toAscList m

getClef :: [Event a] -> Music
getClef eventLs = Clef Treble

controlPatternConverter :: String -> Maybe ControlPattern
-- Note, ControlPattern = Pattern ValueMap
controlPatternConverter inputTidalStr = case parseTidal inputTidalStr of
  Left err -> Nothing
  Right tidalPat -> Just tidalPat

type Cycle = ((Integer, Integer), Maybe Pitch, [Event [ValueMap]])

cycleToNotes :: Cycle -> Maybe [Music]
cycleToNotes cycle@((num, den), mPitch, eventLs) =
  let pitchMap = getPitchMap <$> mPitch
   in foldr (f . eventToMusic pitchMap num den) (Just []) eventLs
  where
    f x b = do
      b' <- b
      x' <- x
      return $ x' ++ b'

getInfo :: Time -> ControlPattern -> Maybe Cycle
getInfo st tidalPat =
  let eventLs1 = offsetArc st <$> queryArc tidalPat (Arc st (st + 1))
   in do
        eventLs <- tail . insertRests <$> (getFullRep <$> getChordRep eventLs1)
        let subdivisions = map part eventLs
            timeSig = getTimeSig subdivisions
            key = getKeySig eventLs1
         in return (timeSig, key, eventLs)

offsetArc :: Rational -> Event a -> Event a
offsetArc st e =
  let x = start $ part e
      y = stop $ part e
      xw = start $ fromJust $ whole e
      yw = stop $ fromJust $ whole e
   in e {part = Arc {start = x - st, stop = y - st},
   whole = Just $ Arc {start = xw - st, stop = yw - st}}

translateOneCycle :: Time -> String -> Maybe Music
translateOneCycle t str = do
  cp <- controlPatternConverter str
  info <- getInfo t cp
  notes <- cycleToNotes info
  return $ case info of
    ((n, d), Nothing, _) -> Sequential $ Time n d : Drums : [Sequential notes]
    ((n, d), Just p, _) -> Sequential $ Time n d : Key p Major : notes
