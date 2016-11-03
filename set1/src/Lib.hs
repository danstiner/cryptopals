module Lib
    ( someFunc
    , hexStringToBase64String
    , hexStringToBytes
    , xor
    , tests
    ) where

import Test.HUnit
import Test.HUnit.Approx
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.String
import Data.Either.Unwrap
import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map

type HexString = String
type Base64String = String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- from http://en.algoritmy.net/article/40379/Letter-frequency-English
englishLetterFrequencies :: Map.Map String Float
englishLetterFrequencies = Map.fromList
  [ ("A", 8.167 / 100)
  , ("B", 1.492 / 100)
  , ("C", 2.782 / 100)
  , ("D", 4.253 / 100)
  , ("E", 12.702 / 100)
  , ("F", 2.228 / 100)
  , ("G", 2.015 / 100)
  , ("H", 6.094 / 100)
  , ("I", 6.966 / 100)
  , ("J", 0.153 / 100)
  , ("K", 0.772 / 100)
  , ("L", 4.025 / 100)
  , ("M", 2.406 / 100)
  , ("N", 6.749 / 100)
  , ("O", 7.507 / 100)
  , ("P", 1.929 / 100)
  , ("Q", 0.095 / 100)
  , ("R", 5.987 / 100)
  , ("S", 6.327 / 100)
  , ("T", 9.056 / 100)
  , ("U", 2.758 / 100)
  , ("V", 0.978 / 100)
  , ("W", 2.360 / 100)
  , ("X", 0.150 / 100)
  , ("Y", 1.974 / 100)
  , ("Z", 0.074 / 100)
  ]

test_englishLetterFrequencies_total_probability = TestCase $ assertApproxEqual
  "Total probability should be 1, within margin of rounding"
  0.0001
  1
  (Map.foldr (+) 0 englishLetterFrequencies)


xor :: B.ByteString -> B.ByteString -> B.ByteString
xor a b = B.pack (B.zipWith Bits.xor a b)

hexStringToBase64String :: HexString -> Either String Base64String
hexStringToBase64String input = bytesToBase64String <$> hexStringToBytes input

hexStringToBytes :: HexString -> Either String B.ByteString
hexStringToBytes = base16DecodeCompletely . fromString

hexStringToBytes' :: HexString -> B.ByteString
hexStringToBytes' = fromRight . hexStringToBytes

bytesToBase64String :: B.ByteString -> Base64String
bytesToBase64String = C.unpack . Base64.encode

base16DecodeCompletely :: B.ByteString -> Either String B.ByteString
base16DecodeCompletely = toMaybe . Base16.decode
  where
    toMaybe (result, leftovers)
      | B.null leftovers = Right result
      | otherwise = Left ("Non-base16 input starting at byte " ++ show (B.length result))

test_set1_challenge1 = expected ~=? actual
  where
    expected = Right "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    actual = hexStringToBase64String "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

test_set1_challenge2 = expected ~=? actual
  where
    actual = xor <$> hexStringToBytes "1c0111001f010100061a024b53535009181c" <*> hexStringToBytes "686974207468652062756c6c277320657965"
    expected = hexStringToBytes "746865206b696420646f6e277420706c6179"

test_set1_challenge3 = undefined
  where
    input = hexStringToBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

tests = TestLabel "Lib" $ TestList
  [ test_set1_challenge1
  , test_set1_challenge2
  , test_englishLetterFrequencies_total_probability
  ]
