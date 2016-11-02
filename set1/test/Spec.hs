import Test.HUnit
import Control.Monad (void)

import Lib

test_set1_challenge1 = expected ~=? actual
  where
    expected = Right "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    actual = hexStringToBase64String "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

main :: IO ()
main = void $ runTestTT test_set1_challenge1
