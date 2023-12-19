module ParseConversionCmd where

import Control.Applicative ()
import Control.Monad
import Data.Map (Map, empty, lookup)
import Data.Maybe
import Data.Music.Lilypond as L
import Data.Ratio
import Data.Vector qualified as V
import GHC.Generics
import GHC.Real (denominator, numerator)
import Language.Haskellish (string)
import Language.Haskellish qualified as Language
import Parse
import Sound.Tidal.Context (pS)
import Sound.Tidal.Pattern as Pattern (ControlPattern)
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import Text.Parsec
  ( ParseError,
    Parsec,
    anyChar,
    digit,
    lookAhead,
    many1,
    oneOf,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Token qualified as Text.Parsec
import Text.Pretty

data Option
  = TimeSignatureBase Integer
  | CP Integer String
  | Silence Integer
  | Advance
  | PrintComp
  | Quit
  | Undo
  deriving (Show, Eq)

setOptionParser :: Parsec String () Option
-- Uses applicatives to parse the command line options
setOptionParser =
  Text.Parsec.lookAhead $
    -- TimeSignatureBase
    --   <$> ( Text.Parsec.string "time-signature-base"
    --           <* simpleWhitespace
    --           *> ParseConversionCmd.integer
    --       )
    -- <|> 
      Advance <$ Text.Parsec.string "advance"
      <|> CP
        <$> ( Text.Parsec.string "d"
                *> ParseConversionCmd.integer
                <* simpleWhitespace
                <* Text.Parsec.string "$"
            )
        <* simpleWhitespace
        <*> Text.Parsec.many1 Text.Parsec.anyChar
      <|> Silence
        <$> ( Text.Parsec.string "silence"
                *> simpleWhitespace
                *> Text.Parsec.string "d"
                *> ParseConversionCmd.integer
            )
      <|> PrintComp <$ Text.Parsec.string "print-comp"
      <|> Quit <$ Text.Parsec.string "quit"
      <|> Undo <$ Text.Parsec.string "undo"

simpleWhitespace :: Parsec String () ()
simpleWhitespace = void $ many1 (oneOf " \t\n")

extractCP :: String -> ControlPattern
extractCP s = case parseTidal s of
  Left _ -> error "Could not parse Tidal pattern"
  Right cp -> cp

integer :: Parsec String () Integer
integer = read <$> many1 digit

parseCommand :: String -> Either ParseError Option
parseCommand = parse setOptionParser ""

-- >>> parse setOptionParser "" "time-signature-base   2 "
-- Right (TimeSignatureBase 2)

-- >>> parse setOptionParser "" "advance    2  "
-- Right (Advance 2)

-- >>> parse setOptionParser "" "slurs"
-- Right Slurs

-- >>> parse setOptionParser "" "no-slurs"
-- Right NoSlurs

-- >>> parse setOptionParser "" "d2    $    s    \"bd\""
-- Right (CP 2 (0>1)|s: "bd")
-- >>> parse setOptionParser "" "d2 $ s \"[bd bd bd] bd bd\""
-- Right (CP 2  (0>⅑)|s: "bd"
-- (⅑>²₉)|s: "bd"
-- (²₉>⅓)|s: "bd"
--  (⅓>⅔)|s: "bd"
--  (⅔>1)|s: "bd")
