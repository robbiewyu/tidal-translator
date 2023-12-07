module TidalToLily
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

{-
  Current naive implementation of retrieving the time signature 
  from the ControlPattern's subdivisions.
-}
getTimeSig :: [ArcF Time] -> L.Music
getTimeSig subdivisions =
  -- find the smallest common denominator of all the subdivisions
  let denominators = map (\(Arc start finish) -> denominator start) subdivisions
      largestCommonDenominator = foldl lcm 1 denominators
      in Time largestCommonDenominator 8 -- TODO: for now, eighth note is the base unit of time

{-
  Current naive implementation of retrieving the key signature 
  from the ControlPattern's subdivisions.
-}
getKeySig :: [Event a] -> Music
getKeySig eventLs = Key (Pitch (C, 0, 5)) Major


getNotes :: [Event a] -> [Music]
getNotes eventLs =
  case getTimeSig $ map (\(Event _ _ arc _) -> arc) eventLs of
    Time num den ->
      concatMap (\(Event ctx whole part val) ->
        let (Arc start finish) = part
            duration = finish - start
            duration' = (numerator duration * num) % (denominator duration * den)
            in
              if isStrange duration' then
                let firstNote = beginSlur $ head $ multiNote duration'
                    lastNote = endSlur $ last $ multiNote duration'
                    in
                      [firstNote] ++ tail (init $ multiNote duration') ++ [lastNote]
              else
                [L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration duration')) []]
      ) eventLs
    _ -> error "Time signature not found"

{-
  Create a slur of notes of power 2 that add up to duration. Similar
  to the DP coin change problem.
  TODO:
  An alternative to creating slurs is to create tuplets. For the future
  use the List monad to generate both slurs and tuplets options as a 
  list of possible Music. Then, use a heuristic to choose the best option.
-}
multiNote :: Rational -> [Music]
multiNote duration = 
  let duration' = numerator duration % denominator duration
      in
        if duration' == 0 then
          []
        else if duration' == 1 then
          [L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration duration)) []]
        else
          let power = floor $ logBase 2 (fromRational duration')
              dur :: Rational = if power < 0 then 1 % (2 ^ (-1 * power)) else 2 ^ power
              note = L.Note (NotePitch (Pitch (C, 0, 5)) Nothing) (Just (Duration dur)) []
              in
                note : multiNote (duration - dur)

{- 
  A duration is strange if it is not a power of 2 (negative exponents allowed).
-}
isStrange :: Rational -> Bool
isStrange duration = 
  let n = numerator duration
      d = denominator duration
      in
        if n == 1 then
          (d /= 1) && (odd d || isStrange (n % (d `div` 2)))
        else
          odd n || isStrange ((n `div` 2) % d)



getClef :: [Event a] -> Music
getClef eventLs = Clef Treble

withinSubset :: ControlPattern -> Bool
withinSubset _ = True -- TODO


controlPatternConverter :: String -> ControlPattern 
-- Note, ControlPattern = Pattern ValueMap
controlPatternConverter inputTidalStr =  
  let tidalPattern = parseTidal inputTidalStr
  in
    case tidalPattern of
      Left err -> error $ show err
      Right tidalPat -> 
        if withinSubset tidalPat then tidalPat 
        else error "Tidal pattern not within subset"


tidalToLilypond :: ControlPattern -> Music
tidalToLilypond tidalPat =
  let eventLs = queryArc tidalPat (Arc 0 1)
      subdivisions = map part eventLs
      in
      let
        timeSig = getTimeSig subdivisions
        clef = getClef eventLs
        notes = getNotes eventLs
  in
    Sequential [clef, timeSig, Sequential notes]