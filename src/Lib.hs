module Lib
  ( someFunc,
  )
where

import Control.Applicative ()
import Data.Maybe
import Parse
import Sound.Tidal.Pattern as Pattern
import GHC.Generics
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import Text.Parsec (ParseError)
import GHC.Real (denominator, numerator)

data TidalParseError = TidalParseError
  { parsecError :: ParseError,
    code :: String
  }

someFunc :: String
someFunc = "Hello CIS 5520"

-- | Parses a line into a TPat and synth name
lineParser :: String -> Maybe (TPat a, String)
lineParser = undefined

-- | INTERNAL LILYPOND REPRESENTATION

-- Note, each voice will be scored on its own staff
type LMeasure = [LVoice]

data LVoice
  = Melody {timeSignature :: (Int, Int), keySignature :: LKey, notes :: [LUnit]}
  | Perc {timeSignature :: (Int, Int), rhythm :: [String]} 
  deriving (Eq, Show)

data LKey = An | As | Bn | Cn | Cs | Dn | Ds | En | Fn | Fs | Gn | Gs deriving (Eq, Show)

data LUnit
  = Rest Int
  | Note (LNote, Int)
  | Chord ([LNote], Int)
  deriving (Eq, Show)

type LNote = (LLetter, Int, Maybe LAccidental)

data LAccidental = Flat | Sharp | Natural deriving (Eq, Show)

data LLetter
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq, Show)

-- | Takes an LMeasure and writes a corresponding Lilypond file
makeLFile :: LMeasure -> IO ()
makeLFile = undefined

-- | TIDAL REPRESENTATION
-- The subset of Tidal that we will be able to parse
data TPat a where
  TPatAtom :: Maybe ((Int, Int), (Int, Int)) -> a -> TPat a
  TPatFast :: TPat Time -> TPat a -> TPat a
  TPatSlow :: TPat Time -> TPat a -> TPat a
  TPatCycleChoose :: Int -> [TPat a] -> TPat a
  TPatStack :: [TPat a] -> TPat a
  --   TPat_Polyrhythm :: Maybe (TPat Rational) -> [TPat a] -> TPat a
  TPatSeq :: [TPat a] -> TPat a
  TPatSilence :: TPat a
  TPatRepeat :: Int -> TPat a -> TPat a
  TPatEnumFromTo :: TPat a -> TPat a -> TPat a
  --   TPat_Chord ::
  --     (Num b, Enum b, Parseable b, Enumerable b) =>
  --     (b -> a) ->
  --     TPat b ->
  --     TPat String ->
  --     [TPat [Modifier]] ->
  --     TPat a
  TPatVar :: String -> TPat a

-- | TRANSLATION FUNCTIONS

-- | Takes a TPat and converts it into a Lilypond voice
translateTPat :: TPat a -> LVoice
translateTPat tpat = case tpat of
  -- TODO: handle time signatures, key signatures, mathematical details, etc.
  TPatAtom Nothing a -> Perc (4, 4) (translateAtom a)
  TPatAtom (Just (num, denom)) a -> Melody num Cn [Rest 1]
  TPatFast tpat' a -> translateTPat tpat'
  TPatSlow tpat' a -> translateTPat tpat'
  TPatCycleChoose n tpat' -> Perc (4, 4) (translateCycleChoose n tpat') -- list/state
  TPatStack tpat' -> Perc (4, 4) (translateStack tpat')
  TPatSeq tpat' -> Perc (4, 4) (translateSeq tpat')
  TPatSilence -> Perc (4, 4) translateSilence
  TPatRepeat n tpat' -> Perc (4, 4) (translateRepeat n tpat')
  TPatEnumFromTo tpat' tpat'' -> Perc (4, 4) (translateEnumFromTo tpat' tpat'')
  TPatVar s -> Perc (4, 4) (translateVar s)

translateAtom :: a -> [String]
translateAtom = undefined

translateCycleChoose :: Int -> [TPat a] -> [String]
translateCycleChoose = undefined

translateStack :: [TPat a] -> [String]
translateStack = undefined

translateSeq :: [TPat a] -> [String]
translateSeq = undefined

translateSilence :: [String]
translateSilence = undefined

translateRepeat :: Int -> TPat a -> [String]
translateRepeat = undefined

translateEnumFromTo :: TPat a -> TPat a -> [String]
translateEnumFromTo = undefined

translateVar :: String -> [String]
translateVar = undefined

-- We will use a helper to convert a ControlPattern (Event ValueMap) into
-- time-value pairs

-- | TESTING
testCheckValid :: Test
testCheckValid =
  TestList
    [ "Invalid 1" ~: isNothing (lineParser "d1 $ s \"bass:5*8\" # lpf (range 100 1000 $ sine)") ~?= True,
      "Invalid 2" ~: isNothing (lineParser "d1 $ n (slow 2 $ fmap (*7) $ run 8) # s \"supergong\" # decay \"[1 0.2]/4\" # voice \"[0.5 0]/8\"") ~?= True,
      "Valid 1" ~: isNothing (lineParser "d1 $ n \"[[bd sd] bd bd]\"") ~?= False,
      "Valid 2" ~: isNothing (lineParser "d2 $ n \"<[e e f g] [g f e _]>\" # sound \"superpiano\"") ~?= False
    ]

-- instance Arbitrary TPat a where
--   arbitrary = undefined

-- | for round-trip testing. Will generate a tiny subset of lilypond voices such that
-- they're an (almost) unique LilyPond correspondent to their corresponding Tidal expression
instance Arbitrary LVoice where
  arbitrary = undefined

-- | for round-trip testing. For any LVoice that belongs to the tiny subset generatable
-- by the above, it converts it into a Tidal expression and otherwise gives Nothing
voiceToLine :: LVoice -> Maybe String
voiceToLine = undefined

-- prop_miniRoundTrip :: LVoice -> Bool
-- prop_miniRoundTrip voice =
--   let convertible = isJust (voiceToLine voice)
--    in not convertible && (lineParser (fromJust (voiceToLine voice)) == voice)

-- not $ isNothing (voiceToLine voice) ==> (lineParser . voiceToLine voice == voice)

-- Not an immediate goal, but we might use some advanced I/O to
-- test that converting a Tidal expression to MIDI via the Tidal application
-- yields the same result as converting the translated LilyPond file into MIDI


type TimeSig = (Int, Int) -- (numerator, denominator)

{--
  Current naive implementation of retrieving the time signature 
  from the ControlPattern's subdivisions.
--}
getTimeSig :: [ArcF Time] -> TimeSig
getTimeSig subdivisions =
  -- find the smallest common denominator of all the subdivisions
  let denominators :: [Int] = map (\(Arc start finish) -> fromIntegral $ denominator start) subdivisions
      largestCommonDenominator = foldl lcm 1 denominators
      -- get the largest factor from 2 - 9 that divides largestCommonDenominator
      largestFactor = head $ filter (\x -> largestCommonDenominator `mod` x == 0) [2..9]
      in
        -- (largestFactor, 8)
        (largestCommonDenominator, largestCommonDenominator) -- TODO: for now, quarter note is the base unit of time

getNotes :: [Event a] -> [LUnit]
getNotes eventLs =
  let (numTimeSig, denTimeSig) = getTimeSig $ map (\(Event _ _ arc _) -> arc) eventLs
    in
      map (\e@(Event ctx whole part val) ->
        let (Arc start finish) = part
            duration = finish - start
            len = fromIntegral $ quot numTimeSig duration
            in
              Lib.Note ((C, 4, Nothing), 1) -- TODO: what does 4 and 1 mean, how are they different?
      ) eventLs



withinSubset :: ControlPattern -> Bool
withinSubset _ = True -- TODO


controlPatternConverter :: String -> ControlPattern -- ControlPattern = Pattern ValueMap
controlPatternConverter inputTidalStr =
  let tidalPattern = parseTidal inputTidalStr
  in
    case tidalPattern of
      Left err -> error $ show err
      Right tidalPat -> if withinSubset tidalPat then tidalPat else error "Tidal pattern not within subset"

tidalToLilypond :: ControlPattern -> LVoice
tidalToLilypond tidalPat =
  let eventLs = queryArc tidalPat (Arc 0 1)
      -- get all part from eventLs (i.e. accumulate part eventL for each eventL in eventLs)
      subdivisions = map part eventLs
      timeSig = getTimeSig subdivisions
      notes = getNotes eventLs
  in
    Melody timeSig Cn [Rest 1] -- TODO

