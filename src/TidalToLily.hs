module TidalToLily (tidalToLilypond, controlPatternConverter)
where

import Control.Applicative ()
import Data.Maybe
import Parse
import Sound.Tidal.Pattern as Pattern
import GHC.Generics
import Test.HUnit
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import Text.Parsec (ParseError)
import GHC.Real (denominator, numerator)
import Text.Pretty
import Data.Music.Lilypond as L
import Data.Ratio

{--
  Current naive implementation of retrieving the time signature 
  from the ControlPattern's subdivisions.
--}
getTimeSig :: [ArcF Time] -> L.Music
getTimeSig subdivisions =
  -- find the smallest common denominator of all the subdivisions
  let denominators = map (\(Arc start finish) -> denominator start) subdivisions
      largestCommonDenominator = foldl lcm 1 denominators
      -- get the largest factor from 2 - 9 that divides largestCommonDenominator
      largestFactor = head $ filter (\x -> largestCommonDenominator `mod` x == 0) [2..9]
      in
        -- (largestFactor, 8)
        Time largestCommonDenominator 8 -- TODO: for now, quarter note is the base unit of time

getKeySig :: [Event a] -> Music
getKeySig eventLs = Key (Pitch (C, 0, 5)) Major

getNotes :: [Event a] -> [Music]
getNotes eventLs =
  case getTimeSig $ map (\(Event _ _ arc _) -> arc) eventLs of
    Time num den ->
      map (\(Event ctx whole part val) ->
        let (Arc start finish) = part
            duration = finish - start 
            duration' = (numerator duration * num) % (denominator duration * den)
            in
              L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration duration')) []
      ) eventLs
    _ -> error "Time signature not found"

getClef :: [Event a] -> Music
getClef eventLs = Clef Treble

withinSubset :: ControlPattern -> Bool
withinSubset _ = True -- TODO


controlPatternConverter :: String -> ControlPattern -- ControlPattern = Pattern ValueMap
controlPatternConverter inputTidalStr =  let tidalPattern = parseTidal inputTidalStr
  in
    case tidalPattern of
      Left err -> error $ show err
      Right tidalPat -> if withinSubset tidalPat then tidalPat else error "Tidal pattern not within subset"


tidalToLilypond :: ControlPattern -> Music
tidalToLilypond tidalPat =
  let eventLs = queryArc tidalPat (Arc 0 1)
      -- get all part from eventLs (i.e. accumulate part eventL for each eventL in eventLs)
      subdivisions = map part eventLs
      in 
      let 
        timeSig = getTimeSig subdivisions
        clef = getClef eventLs
        notes = getNotes eventLs
  in
    Sequential [clef, timeSig, Sequential notes]