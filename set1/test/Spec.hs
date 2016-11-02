import Test.HUnit
import Control.Monad (void)

import Lib

test_set1_challenge1 = expected ~=? actual
  where
    expected = Right "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    actual = hexStringToBase64String "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

test_set1_challenge2 = expected ~=? actual
  where
    actual = xor <$> hexStringToBytes "1c0111001f010100061a024b53535009181c" <*> hexStringToBytes "686974207468652062756c6c277320657965"
    expected = hexStringToBytes "746865206b696420646f6e277420706c6179"

tests = TestList [test_set1_challenge1, test_set1_challenge2]

main :: IO ()
main = void $ runTestTT tests
