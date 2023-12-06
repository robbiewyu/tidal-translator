module Main where

import TidalToLily
import Text.Pretty
import Data.Music.Lilypond as L

main :: IO ()
main = do 
    putStrLn "Enter a Tidal pattern: "
    -- Take in a Tidal pattern
    tidalPattern <- getLine
    -- Convert the Tidal pattern to a Lilypond pattern
    let convertedLilypondAST = tidalToLilypond (controlPatternConverter tidalPattern)
    -- Print the Lilypond pattern
    print $ pretty convertedLilypondAST
    -- TODO Run command line command to convert Lilypond pattern to PDF and store in "examples" folder 
    -- relative to the current directory

