module Main where

import Data.Maybe
import Data.Music.Lilypond as L
import ParseConversionCmd
import Sound.Tidal.Pattern as Pattern (ControlPattern, Time)
import System.IO as S
import Text.Pretty
import TidalToLily
import TranslationStepper2 (writeAndScore)

main :: IO ()
main = do
  putStrLn "Enter a Tidal pattern: "
  cmd <- getLine
  let
    controlPattern = case parseCommand cmd of
      Left _ -> error "Could not parse command"
      Right (CP _ cp) -> cp
      Right _ -> error "Command unsupported"
    translation = translateOneCycle (0 :: Time) controlPattern
    in
      case translation of
        Just m -> do
          putStrLn "Success:"
          print $ pretty m
          writeAndScore "outputs/output.ly" m
        Nothing -> do
          putStrLn "Failure"

