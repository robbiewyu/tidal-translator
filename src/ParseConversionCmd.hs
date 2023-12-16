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
import Parse
import Sound.Tidal.Pattern as Pattern (ControlPattern)
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import Text.Parsec (ParseError, parse, 
            Parsec, (<|>), oneOf, many1, digit,
            string, anyChar, try, lookAhead)
import Text.Pretty
import Sound.Tidal.Context (pS)
import Language.Haskellish (string)
import qualified Language.Haskellish as Language
import qualified Text.Parsec.Token as Text.Parsec


data Option
  = Slurs
  | NoSlurs
  | TimeSignatureBase Integer
  | CP Integer ControlPattern
  | Advance Integer
  deriving (Show, Eq)

setOptionParser :: Parsec String () Option
-- Uses applicatives to parse the command line options
setOptionParser =
  Text.Parsec.lookAhead $ 
    Slurs <$ Text.Parsec.string "slurs"
    <|> NoSlurs <$ Text.Parsec.string "no-slurs"
    <|> TimeSignatureBase <$> (Text.Parsec.string "time-signature-base" 
                <* simpleWhitespace *> ParseConversionCmd.integer)
    <|> Advance <$> (Text.Parsec.string "advance" 
                <* simpleWhitespace *> ParseConversionCmd.integer)
    <|> CP <$> (Text.Parsec.string "d" *> ParseConversionCmd.integer 
                <* simpleWhitespace <* Text.Parsec.string "$")
                <* simpleWhitespace
                <*> (extractCP <$> Text.Parsec.many1 Text.Parsec.anyChar)

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
