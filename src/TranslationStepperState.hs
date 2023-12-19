module TranslationStepperState where

import Control.Monad (unless, when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Internal.Sorting (QList (Nil))
import State (State)
import State qualified as S
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)
import LilypondPlus.LilypondPlus (Pitch)
import LilypondPlus.LilypondPlus qualified as LP
import Data.Data (Data)
import Sound.Tidal.Context (Pattern(Pattern))
import ParseConversionCmd (parseCommand, Option (..))

-- OBSOLETE

-- | A type for representing the current state of the translation.
type Store = Map Int DataValue

data DataValue = 
    PatternValue { 
        patternString :: String,
        startTime :: Int
    }
    deriving (Eq, Ord, Show)

initialStore :: Store
initialStore = Map.empty

extendedStore :: Store
extendedStore = Map.fromList [
    (1, PatternValue "s $ \"bd bd\"" 0),
    (2, PatternValue "n $ \"c5 c5 [c5 c4]\"" 1)
    ]

index :: Int -> State Store DataValue
index i = do
  store <- S.get
  case store !? i of
    Just v -> return v
    Nothing -> error $ "Could not find pattern d" ++ show i ++ " in store"


test_index :: Test
test_index =
  "index tests"
    ~: TestList
      [ 
        "index d1" ~: S.evalState (index 1) extendedStore ~?= PatternValue "s $ \"bd bd\"" 0,
        "index d2" ~: S.evalState (index 2) extendedStore ~?= PatternValue "n $ \"c5 c5 [c5 c4]\"" 1
      ]

-- >>> runTestTT test_index

-- >>> S.evalState (index 1) extendedStore
-- >>> S.evalState (index 2) extendedStore
-- >>> S.evalState (index 3) extendedStore

-- if v is Nothing, it's a delete
update :: Integer -> Maybe DataValue -> State Store ()
update n v = do
  store <- S.get
  S.put $ Map.alter (const v) (fromIntegral n) store

test_update :: Test
test_update = 
    "update tests"
    ~: TestList 
    [
        "update d1" ~: S.execState (update xref (PatternValue "s $ \"bd bd bd\"" 0)) extendedStore ~?= Map.fromList [
            (PatternName "d1", PatternValue "s $ \"bd bd bd\"" 0),
            (PatternName "d2", PatternValue "n $ \"c5 c5 [c5 c4]\"" 1)
            ],
        "update d2" ~: S.execState (update yref (PatternValue "n $ \"c5 c5 c5 [c5 c4]\"" 2)) extendedStore ~?= Map.fromList [
            (PatternName "d1", PatternValue "s $ \"bd bd\"" 0),
            (PatternName "d2", PatternValue "n $ \"c5 c5 c5 [c5 c4]\"" 2)
            ],
        "update d3" ~: S.execState (update (PatternName "d3") (PatternValue "n $ \"c5 c5 c5 [c5 c4]\"" 4)) extendedStore ~?= Map.fromList [
            (PatternName "d1", PatternValue "s $ \"bd bd\"" 0),
            (PatternName "d2", PatternValue "n $ \"c5 c5 [c5 c4]\"" 1),
            (PatternName "d3", PatternValue "n $ \"c5 c5 c5 [c5 c4]\"" 4)
            ]
    ]

-- >>> runTestTT test_update
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-- >>> S.execState (update (PatternName "d3") (PatternValue "n $ \"c5 c5 c5 [c5 c4]\"" 1 0 1)) extendedStore 
-- Could not find pattern d3 in store

----------
-- | Main top-level loop

data Stepper = Stepper
  { store :: Store,
    history :: Maybe Stepper,
    cycleIndex :: Integer
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { store = initialStore,
      history = Nothing,
      cycleIndex = 0
    }


stepper :: IO ()
stepper = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      res <- prompt
      case res of
        Advance n -> go $ Stepper {store = store ss, history = Just ss, cycleIndex = cycleIndex ss + 1}
        CP i s -> let
          state = do
            S.put (store ss)
            update i (Just (PatternValue s (fromIntegral $ cycleIndex ss)))
          store' = S.execState state (store ss)
          in undefined
        _ -> undefined 
        

prompt :: IO Option
prompt = do
  putStrLn "Enter a command"
  cmd <- getLine
  case parseCommand cmd of
    Left _ -> do
      putStrLn "Could not parse command"
      prompt
    Right x -> return x