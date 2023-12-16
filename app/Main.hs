module Main where

import TidalToLily
import Text.Pretty
import Data.Music.Lilypond as L
import ParseConversionCmd
import Sound.Tidal.Pattern as Pattern (ControlPattern)

main :: IO ()
main = do 
    putStrLn "Enter a Tidal pattern: "
    -- Take in a Tidal pattern
    cmd <- getLine
    let controlPattern = case parseCommand cmd of
            Left _ -> error "Could not parse command"
            Right (CP _ cp) -> cp
            Right _ -> error "Command unsupported"
    -- Convert the Tidal pattern to a Lilypond pattern
        convertedLilypondAST = tidalToLilypond controlPattern
    -- Print the Lilypond pattern
    print $ pretty convertedLilypondAST
    -- TODO Run command line command to convert Lilypond pattern to PDF and store in "examples" folder 
    -- relative to the current directory

