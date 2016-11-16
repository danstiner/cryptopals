{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( decodeHex
    , tests
    ) where

import Data.FileEmbed
import qualified Data.Either as Either
import qualified Data.List as List
import Test.HUnit
import Test.HUnit.Approx
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Data.String
import Data.Either.Unwrap
import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ratio
import Control.Arrow
import Data.Function
import Data.Maybe
import System.IO.Unsafe
import Data.Char as Char

type HexString = String
type Base64String = String
type Frequency = Ratio Int
type CipherText = ByteString
type PlainText = ByteString
type Key = ByteString
type Score = Float

-- from http://en.algoritmy.net/article/40379/Letter-frequency-English
englishLetterFrequencies :: Map.Map Char Float
englishLetterFrequencies = Map.fromList
  [ ('A', 8.167 / 100)
  , ('B', 1.492 / 100)
  , ('C', 2.782 / 100)
  , ('D', 4.253 / 100)
  , ('E', 12.702 / 100)
  , ('F', 2.228 / 100)
  , ('G', 2.015 / 100)
  , ('H', 6.094 / 100)
  , ('I', 6.966 / 100)
  , ('J', 0.153 / 100)
  , ('K', 0.772 / 100)
  , ('L', 4.025 / 100)
  , ('M', 2.406 / 100)
  , ('N', 6.749 / 100)
  , ('O', 7.507 / 100)
  , ('P', 1.929 / 100)
  , ('Q', 0.095 / 100)
  , ('R', 5.987 / 100)
  , ('S', 6.327 / 100)
  , ('T', 9.056 / 100)
  , ('U', 2.758 / 100)
  , ('V', 0.978 / 100)
  , ('W', 2.360 / 100)
  , ('X', 0.150 / 100)
  , ('Y', 1.974 / 100)
  , ('Z', 0.074 / 100)
  , ('a', 8.167 / 100)
  , ('b', 1.492 / 100)
  , ('c', 2.782 / 100)
  , ('d', 4.253 / 100)
  , ('e', 12.702 / 100)
  , ('f', 2.228 / 100)
  , ('g', 2.015 / 100)
  , ('h', 6.094 / 100)
  , ('i', 6.966 / 100)
  , ('j', 0.153 / 100)
  , ('k', 0.772 / 100)
  , ('l', 4.025 / 100)
  , ('m', 2.406 / 100)
  , ('n', 6.749 / 100)
  , ('o', 7.507 / 100)
  , ('p', 1.929 / 100)
  , ('q', 0.095 / 100)
  , ('r', 5.987 / 100)
  , ('s', 6.327 / 100)
  , ('t', 9.056 / 100)
  , ('u', 2.758 / 100)
  , ('V', 0.978 / 100)
  , ('w', 2.360 / 100)
  , ('x', 0.150 / 100)
  , ('y', 1.974 / 100)
  , ('z', 0.074 / 100)
  , (' ', 20 / 100)
  ]

distanceFromExpected :: (Ord k, Num b) => (a -> a -> b) -> a -> Map k a -> Map k a -> Map k b
distanceFromExpected metric zero = Map.mergeWithKey combine expectedButNotPresent unexpected
  where
    combine _ x1 x2 = Just $ metric x1 x2
    expectedButNotPresent = Map.map (metric zero)
    unexpected = const Map.empty

frequencies :: String -> Map Char Frequency
frequencies input = Map.map (% totalCharacters) characterCountMap
  where
    totalCharacters = length input
    characterCountMap = Map.fromListWith (+) characterSingletons
    characterSingletons = map (\c -> (c, 1)) input

xor :: ByteString -> ByteString -> ByteString
xor a b = B.pack (B.zipWith Bits.xor a b)

hexStringToBytes :: HexString -> Either String ByteString
hexStringToBytes = base16DecodeCompletely . fromString

hexStringToBytes' :: HexString -> ByteString
hexStringToBytes' = fromRight . hexStringToBytes

decodeHex = hexStringToBytes'

base16DecodeCompletely :: ByteString -> Either String ByteString
base16DecodeCompletely = toMaybe . Base16.decode
  where
    toMaybe (result, leftovers)
      | B.null leftovers = Right result
      | otherwise = Left ("Non-base16 input starting at byte " ++ show (B.length result * 2))

bruteForceEnglishEncryptedWithSingleByteXOR :: CipherText -> [(PlainText, Score)]
bruteForceEnglishEncryptedWithSingleByteXOR cipertext =
    map score . filter isPrintable . map (decrypt cipertext) $ keys
  where
    keys :: [Key]
    keys = map B.singleton [0..255]
    score :: PlainText -> (PlainText, Score)
    score text = (text, score' text)
    score' text = distance englishLetterFrequencies (Map.map fromFrequency (frequencies (C.unpack text)))
    distance expected actual = sumMap $ distanceFromExpected metric zero expected actual
    sumMap = Map.foldl' (+) 0
    metric x1 x2 = abs (x1 - x2)
    zero = 0.0
    fromFrequency = fromRational . toRational
    isPrintable :: ByteString -> Bool
    isPrintable = C.all ((||) <$> Char.isPrint <*> Char.isSpace)
    decrypt = flip decryptWithRepeatingKeyXOR

encryptWithRepeatingKeyXOR :: Key -> PlainText -> CipherText
encryptWithRepeatingKeyXOR key = B.concat . map (`xor` key) . chunks (B.length key)

decryptWithRepeatingKeyXOR :: Key -> CipherText -> PlainText
decryptWithRepeatingKeyXOR = encryptWithRepeatingKeyXOR

chunks :: Int -> ByteString -> [ByteString]
chunks size = loop 0
  where
    loop offset bs
      | B.null bs = []
      | otherwise = B.take size bs : loop (offset + size) (B.drop size bs)

hammingDistance :: ByteString -> ByteString -> Int
hammingDistance bs1 bs2 = popCount $ difference bs1 bs2
  where
    difference = xor
    popCount = B.foldl' (\acc byte -> acc + Bits.popCount byte) 0

test_set1_challenge1 = expected ~=? actual
  where
    expected = C.pack "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    actual = Base64.encode $ decodeHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

test_set1_challenge2 = expected ~=? actual
  where
    expected = decodeHex "746865206b696420646f6e277420706c6179"
    actual = decodeHex "1c0111001f010100061a024b53535009181c" `xor` decodeHex "686974207468652062756c6c277320657965"

test_set1_challenge3 =
  C.pack "Cooking MC's like a pound of bacon" ~=? fst (List.minimumBy (compare `on` snd) $ bruteForceEnglishEncryptedWithSingleByteXOR (decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))

test_set1_challenge4 = C.pack "Now that the party is jumping\n" ~=? fst (List.minimumBy (compare `on` snd) decodedLines)
  where
    decodedLines :: [(PlainText, Score)]
    decodedLines = concatMap (bruteForceEnglishEncryptedWithSingleByteXOR . decodeHex) testLines
    testLines = lines $(embedStringFile "data/4.txt")

test_set1_challenge5 = expected_ciphertext ~=? encryptWithRepeatingKeyXOR key plaintext
  where
    plaintext = C.pack . List.intercalate "\n" $
      [ "Burning 'em, if you ain't quick and nimble"
      , "I go crazy when I hear a cymbal"]
    key = C.pack "ICE"
    expected_ciphertext = decodeHex $
       "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272" ++ 
       "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

test_hammingDistance = C.pack "this is a test" `hammingDistance` C.pack "wokka wokka!!!" ~=? 37

tests = TestLabel "Lib" $ TestList
  [ test_set1_challenge1
  , test_set1_challenge2
  , test_set1_challenge3
  , test_set1_challenge4
  , test_set1_challenge5
  , test_hammingDistance
  ]
