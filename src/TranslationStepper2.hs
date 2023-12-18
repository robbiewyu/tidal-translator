module TranslationStepper2 where

import Control.Monad (unless, when)
import Data.Data (Data)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence.Internal.Sorting (QList (Nil))
import Data.Vector qualified as V
import LilypondPlus.LilypondPlus (Pitch)
import LilypondPlus.LilypondPlus as L
  ( Music (..),
  )
import LilypondPlus.LilypondPlus qualified as LP
import ParseConversionCmd (Option (..), parseCommand)
import Sound.Tidal.Context (EventF (value), Pattern (Pattern), cP, s, xDefault)
import Sound.Tidal.Pattern as Pattern (ControlPattern)
import System.IO
import System.Process (callCommand)
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Pretty
import Text.Read (readMaybe)
import TidalToLily (Cycle, controlPatternConverter, cycleToMeasure)

-- | A type for representing the current state of the translation.
type Table = Map Int [Cycle]

nCyc :: Int
nCyc = 50

-- | Converts table to full piece
tableToComposition :: Table -> Music
tableToComposition = undefined

-- need to first go through columns and standardize time signature

-- num / denominator is length of measure
-- num = lcm of denominators in durations -> num / 8
-- find lcm of all nums (take out power of 2) and divide by
-- 4 / 4 and 3 / 4. lcm = 12. then 12 / 8 for both
-- 4 / 4 converts to 3 / 4 by  1/4 -> 3 / 16

silentCycles :: Maybe [Cycle]
silentCycles = Just $ replicate nCyc ((1, 1), Nothing, [Rest (Just 1) []])

-- | fill up ith to last cycle in oList with pList
startCycleAt :: Int -> [Cycle] -> [Cycle] -> [Cycle]
startCycleAt i pList oList = take i oList ++ take (nCyc - i) pList

-- >>> startCycleAt 2 [((1,1),Nothing,[Rest (Just 1) []])] [((1,1),Nothing,[Rest (Just 1) []])]
-- [((1,1),Nothing,[Rest (Just (Duration {getDuration = 1 % 1})) []]),((1,1),Nothing,[Rest (Just (Duration {getDuration = 1 % 1})) []])]

update :: Int -> Int -> Maybe [Cycle] -> Table -> Table
update voice cycNum value = Map.alter f voice
  where
    f mv = do
      value' <- value
      return $
        startCycleAt
          cycNum
          value'
          ( case mv of
              Just v -> v
              Nothing -> fromJust silentCycles
          )

getCycleList :: String -> Maybe [Cycle]
getCycleList cp = mapM f [0 .. (nCyc - 1)]
  where
    f i = (controlPatternConverter cp) >>= (cycleToMeasure (toRational i))

----------

-- | Main top-level loop

-- Store should contain parsed versions of each pattern, segmented into cycles
--

-- easiest: each stepper or iteration contains active patterns, parsed into cycleLimit cycles, as well as

data Stepper = Stepper
  { table :: Table,
    history :: Maybe Stepper,
    index :: Int
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { table = Map.empty,
      history = Nothing,
      index = 0
    }

stepper :: IO ()
stepper = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      res <- prompt
      if fst res == Quit
        then printDebug (show $ table ss) >> return ()
        else
          let (msg, table', index') = handleResult res (table ss) (index ss)
           in (putStrLn msg >> (go $ Stepper {table = table', history = Just ss, index = index'}))

handleResult :: (Option, Maybe [Cycle]) -> Table -> Int -> (String, Table, Int)
handleResult (opt, mCyc) table index = case opt of
  Advance -> ("Advanced a cycle", table, index + 1)
  CP i s -> ("Stored pattern d" ++ show i ++ " = " ++ s, update (fromIntegral i) index mCyc table, index)
  Silence i -> ("Silenced pattern d" ++ show i, update (fromIntegral i) index silentCycles table, index)
  PrintComp -> (show table, table, index)
  _ -> error "TODO: handleResult"

prompt :: IO (Option, Maybe [Cycle])
prompt = do
  putStrLn "Enter a command"
  cmd <- getLine
  case parseCommand cmd of
    Left _ -> reprompt "Invalid command"
    Right x@(CP i s) -> case getCycleList s of
      Just y -> return (x, Just y)
      Nothing -> reprompt "Pattern parsing failed. Try again"
    Right x -> return (x, Nothing)

reprompt :: String -> IO (Option, Maybe [Cycle])
reprompt msg = do
  putStrLn msg
  prompt

printDebug :: String -> IO ()
printDebug s = do
  handle <- openFile "debug/debug.txt" WriteMode
  hPutStr handle s
  hClose handle

writeAndScore :: String -> Music -> IO ()
writeAndScore fileName convertedLilypondAST = do
  -- Writes the lilypond file
  handle <- openFile fileName WriteMode
  hPutStr handle "\\version \"2.24.3\" \n { \n"
  hPrint handle convertedLilypondAST
  hPutStr handle "}"
  hClose handle
  -- Converts to actual score as pdf
  callCommand "lilypond -fpdf outputs/output.ly"
