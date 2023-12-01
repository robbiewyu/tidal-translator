module Lib
  ( someFunc,
  )
where

import Control.Applicative ()
import Data.Maybe
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

data TidalParseError = TidalParseError
  { parsecError :: ParseError,
    code :: String
  }

someFunc :: String
someFunc = "Hello CIS 5520"

-- | parses a line into a TPat and synth name
lineParser :: String -> (TPat a, String)
lineParser = undefined

-- -- Verifies that given string is acceptable for our subset of Tidal
-- -- Uses parseBP to parse the string if it is acceptable, otherwise returns
-- -- an error

-- goodStr :: String -> Either String String

-- Note to self on functions to include:
-- validity testing
-- converter from string (or whatever immediate repr Tidal lib has) to LVoice, given target key signature
-- function that runs above on all key signatures and outputs best fit key signature

-- Internal representation for Lilypond format

-- each voice will be scored on its own staff
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

-- | The subset of Tidal that we will be able to parse
data TPat a where
  TPat_Atom :: Maybe ((Int, Int), (Int, Int)) -> a -> TPat a
  TPat_Fast :: TPat Time -> TPat a -> TPat a
  TPat_Slow :: TPat Time -> TPat a -> TPat a
  TPat_CycleChoose :: Int -> [TPat a] -> TPat a
  TPat_Stack :: [TPat a] -> TPat a
  --   TPat_Polyrhythm :: Maybe (TPat Rational) -> [TPat a] -> TPat a
  TPat_Seq :: [TPat a] -> TPat a
  TPat_Silence :: TPat a
  TPat_Repeat :: Int -> TPat a -> TPat a
  TPat_EnumFromTo :: TPat a -> TPat a -> TPat a
  TPat_Var :: String -> TPat a

--   TPat_Chord ::
--     (Num b, Enum b, Parseable b, Enumerable b) =>
--     (b -> a) ->
--     TPat b ->
--     TPat String ->
--     [TPat [Modifier]] ->
--     TPat a

-- TESTING

testCheckValid :: Test
testCheckValid =
  TestList
    [ "Invalid 1" ~: lineParser "d1 $ s \"bass:5*8\" # lpf (range 100 1000 $ sine)" ~?= Nothing,
      "Invalid 2" ~: lineParser "d1 $ n (slow 2 $ fmap (*7) $ run 8) # s \"supergong\" # decay \"[1 0.2]/4\" # voice \"[0.5 0]/8\"" ~?= Nothing,
      "Valid 1" ~: isNothing $ lineParser "d1 $ n \"[[bd sd] bd bd]\"" ~?= False,
      "Valid 2" ~: isNothing $ lineParser "d2 $ n \"<[e e f g] [g f e _]>\" # sound \"superpiano\"" ~?= False
    ]

instance Arbitrary TPat a where
  arbitrary = undefined

-- | for round-trip testing. Will generate a tiny subset of lilypond voices such that
-- they're an (almost) unique LilyPond correspondent to their corresponding Tidal expression
instance Arbitrary LVoice where
  arbitrary = undefined

-- | for round-trip testing. For any LVoice that belongs to the tiny subset generatable
-- by the above, it converts it into a Tidal expression and otherwise gives Nothing
voiceToLine :: LVoice -> Maybe String
voiceToLine = undefined

prop_miniRoundTrip :: LVoice -> Bool
prop_miniRoundTrip voice =
  not isNothing (voiceToLine voice) ==> lineParser . voiceToLine voice == voice

-- Not an immediate goal, but we might use some advanced I/O to
-- test that converting a Tidal expression to MIDI via the Tidal application
-- yields the same result as converting the translated LilyPond file into MIDI