import Test.HUnit
import Test.QuickCheck
import UnitTests as UT
import TidalToLilyCheck as TLC
import qualified UnitTests as UT


main :: IO ()
main = do
  putStrLn "Round trip quickcheck testing"
  TLC.qc 
  putStrLn "Unit Tests"
  UT.test_all
  return ()
