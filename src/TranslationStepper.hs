module TranslationStepper where

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

-- | A type for representing the current state of the translation.
type Store = Map Int DataValue

type DataValue = (String, Int)

initialStore :: Store
initialStore = Map.empty

extendedStore :: Store
extendedStore = Map.fromList [
    (1, ("s $ \"bd bd\"", 0)),
    (2, ("n $ \"c5 c5 [c5 c4]\"", 1))
    ]

-- if v is Nothing, it's a delete
update :: Integer -> Maybe DataValue -> Store -> Store
update n v = Map.alter (const v) (fromIntegral n)

----------
-- | Main top-level loop

data Stepper = Stepper
  { store :: Store,
    history :: Maybe Stepper,
    cycleIndex :: Int
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
      if res == Quit then
        return ()
      else
        let output = case res of
              Advance n -> "Advanced " ++ show n ++ " cycles"
              CP i s -> "Stored pattern d" ++ show i ++ " = " ++ s
              Silence i -> "Silenced pattern d" ++ show i
              PrintComp -> "TODO: Printed composition"
              _ -> error "bad output case"
            (storePrev, cycleIndexPrev) = (store ss, cycleIndex ss)
            (store', cycleIndex') = case res of
                Advance n -> (storePrev, cycleIndexPrev + fromIntegral n)
                CP i s -> (update i (Just (s, cycleIndexPrev)) storePrev, cycleIndexPrev)
                Silence i -> (update i Nothing storePrev, cycleIndexPrev)
                PrintComp -> (storePrev, cycleIndexPrev)
                _ -> error "bad store update case"
            in
              (putStrLn output >> (go $ Stepper {store = store', history = Just ss, cycleIndex = cycleIndex'}))


prompt :: IO Option
prompt = do
  putStrLn "Enter a command"
  cmd <- getLine
  case parseCommand cmd of
    Left _ -> do
      putStrLn "Could not parse command"
      prompt
    Right x -> return x
