import Test.HUnit
import Control.Monad (void)

import qualified Lib

tests = Lib.tests

main :: IO ()
main = void $ runTestTT tests
