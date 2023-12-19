module TranslationStepper2 where

import Control.Monad (unless, when)
import Control.Monad.Cont (void)
import Data.Data (Data)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence.Internal.Sorting (QList (Nil))
import Data.Vector qualified as V
import LilypondPlus.LilypondPlus (Clef (..), Mode (Major), Pitch)
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
import TidalToLily 
  (Cycle, controlPatternConverter, cycleRest, cycleToNotes, getInfo)
import qualified Data.ByteString as BS

-- | A type for representing the current state of the translation.
type Table = Map Int [Cycle]

nCyc :: Int
nCyc = 50



-- | Converts table to full piece
tableToComposition :: Table -> Music
tableToComposition table =
  let lcm = getTotLCM table
      num = bndSimp lcm 1
      denom = lcm `div` num
      emptyMeasure = fromJust $ 
        cycleToNotes ((num, denom), Nothing, [cycleRest])
      voices = map (getVoice (num, denom) emptyMeasure . snd) $ Map.toList table
   in Sequential [Time num denom, Simultaneous False voices]

getTotLCM :: Table -> Integer
getTotLCM table = foldr (\((n, _), _, _) b -> lcm n b) 1 
  (concatMap snd (Map.toList table))

-- can factor out max of 2^7 twos
bndSimp :: Integer -> Int -> Integer
bndSimp x k = if k <= 0 || odd x then x else bndSimp (x `div` 2) (k - 1)

-- for a single voice, how to separate?
getVoice :: (Integer, Integer) -> [Music] -> [Cycle] -> Music
getVoice ts emptyMeasure cs = combine $ foldr f ([], []) cs
  where
    f c@(ks, pit, es) (b1, b2) =
      let res = fromJust (cycleToNotes $ replaceTS ts c)
       in case pit of
            Nothing -> (emptyMeasure ++ b1, res ++ b2)
            Just p -> (Key p Major : res ++ b1, emptyMeasure ++ b2)
    combine (as, bs) = New "Staff" Nothing $ Simultaneous True 
      [Sequential as, Sequential $ Drums : [Sequential bs]]

silentCycles :: Maybe [Cycle]
silentCycles = Just $ replicate nCyc ((1, 1), Nothing, [cycleRest])

replaceTS :: (Integer, Integer) -> Cycle -> Cycle
replaceTS (n, d) (_, b, c) = ((n, d), b, c)

-- | fill up ith to last cycle in oList with pList
startCycleAt :: Int -> [Cycle] -> [Cycle] -> [Cycle]
startCycleAt i pList oList = take i oList ++ take (nCyc - i) pList

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
    f i = do
      cPat <- controlPatternConverter cp
      info <- getInfo (toRational i) cPat
      endRes <- cycleToNotes info
      return info


----------

-- | Main top-level loop

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
        then void 
          (writeAndScore "outputs/stepper_output.ly" 
            (tableToComposition $ table ss))
      else if fst res == Undo
        then case history ss of
          Just h -> go h
          Nothing -> putStrLn "Can't undo" >> go ss
      else if fst res == PrintComp
        then print (table ss) >> go ss
        else
          let (msg, table', index') = handleResult res (table ss) (index ss)
           in (putStrLn msg >> 
            (go $ Stepper {table = table', history = Just ss, index = index'}))

handleResult :: (Option, Maybe [Cycle]) -> Table -> Int -> (String, Table, Int)
handleResult (opt, mCyc) table index = case opt of
  Advance -> ("Advanced a cycle", table, index + 1)
  CP i s -> ("Stored pattern d" ++ show i ++ " = " ++ s, 
    update (fromIntegral i) index mCyc table, index)
  Silence i -> ("Silenced pattern d" ++ show i, 
    update (fromIntegral i) index silentCycles table, index)
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
  putStrLn s
-- printDebug s = do
--   handle <- openFile "debug/debug.txt" WriteMode
--   hPutStr handle s
--   hClose handle

data LilyPondFileAST =
  File [LilyPondFileAST]
  | Score Music
  | Version String
  deriving (Eq, Show)

instance Pretty LilyPondFileAST where
  pretty (File lilypondASTs) = fsep $ map pretty lilypondASTs
  pretty (Version version) = string $ "\\version \"" ++ version ++ "\""
  pretty (Score music) = string "\\score {" 
    <> pretty music 
    <> string "\n \\layout {} \n \\midi {} \n }"

writeAndScore :: String -> Music -> IO ()
writeAndScore fileName convertedLilypondAST = do
  -- Writes the lilypond file
  writeFile fileName ""
  handle <- openFile fileName WriteMode
  let lilypondFileAST = File [Version "2.24.3", Score convertedLilypondAST]
  hPutStr handle $ show $ pretty lilypondFileAST
  hClose handle
  -- Converts to actual score as pdf
  callCommand 
   ("lilypond -dmidi-extension=mid --output=outputs/ " ++ fileName)

