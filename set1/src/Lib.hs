{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , hexStringToBase64String
    , hexStringToBytes
    , hexStringToBytes'
    , xor
    , tests
    ) where

import Data.FileEmbed
import Data.Char
import qualified Data.Either as Either
import qualified Data.List as List
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
import Data.Map.Strict (Map)
import Data.Ratio
import Control.Arrow
import Data.Function
import Data.Maybe
import System.IO.Unsafe

type HexString = String
type Base64String = String
type Frequency = Ratio Int

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
  , ('!', 0)
  , ('"', 0)
  , ('#', 0)
  , ('$', 0)
  , ('%', 0)
  , ('&', 0)
  , ('\'', 0)
  , ('(', 0)
  , (')', 0)
  , ('*', 0)
  , ('+', 0)
  , (',', 0)
  , ('-', 0)
  , ('.', 0)
  , ('/', 0)
  , ('0', 0)
  , ('1', 0)
  , ('2', 0)
  , ('3', 0)
  , ('4', 0)
  , ('5', 0)
  , ('6', 0)
  , ('7', 0)
  , ('8', 0)
  , ('9', 0)
  , (':', 0)
  , (';', 0)
  , ('<', 0)
  , ('=', 0)
  , ('>', 0)
  , ('?', 0)
  , ('@', 0)
  , ('[', 0)
  , ('\\', 0)
  , (']', 0)
  , ('^', 0)
  , ('_', 0)
  , ('`', 0)
  , ('{', 0)
  , ('|', 0)
  , ('}', 0)
  , ('~', 0)
  , ('\n', 0)
  , ('\t', 0)
  , ('\r', 0)
  ]

measureDistance :: (Ord k, Num b) => (a -> a -> b) -> a -> Map k a -> Map k a -> Either [k] b
measureDistance metric zero expected actual = sum <$> resultOrError merged
  where
    resultOrError :: Map k (Maybe a) -> Either [k] [a]
    resultOrError = resultOrError' . Either.partitionEithers . map maybeToEither . Map.toList
    resultOrError' :: ([a], [b]) -> Either [a] [b]
    resultOrError' ([], bs) = Right bs
    resultOrError' (as, _) = Left as
    maybeToEither (k, Nothing) = Left k
    maybeToEither (k, Just x) = Right x
    merged = Map.mergeWithKey distance (Map.map (Just . metric zero)) (Map.map (const Nothing)) expected actual
    distance _ x1 x2 = Just (Just (metric x1 x2))

frequencies :: String -> Map Char Frequency
frequencies input = Map.map overTotal characterCounts
  where
    overTotal :: Int -> Frequency
    overTotal count = count % totalCharacters
    characterCounts = Map.fromListWith (+) (map (\c -> (c, 1)) input)
    totalCharacters = length input

-- convert from hex to characters, compute frequencies accounting for non-present characters, compute distance from average frequency squared
frequencyDistance :: String -> Either String Float
frequencyDistance input = measureDistance metric 0.0 ideal actual
  where
    metric x1 x2 = (x1 - x2) * (x1 - x2)
    ideal = englishLetterFrequencies
    actual = Map.map frequencyToFractional (frequencies input)
    frequencyToFractional = fromRational . toRational

xor :: B.ByteString -> B.ByteString -> B.ByteString
xor a b = B.pack (B.zipWith Bits.xor a b)

hexStringToBase64String :: HexString -> Either String Base64String
hexStringToBase64String input = bytesToBase64String <$> hexStringToBytes input

hexStringToBytes :: HexString -> Either String B.ByteString
hexStringToBytes = base16DecodeCompletely . fromString

hexStringToBytes' :: HexString -> B.ByteString
hexStringToBytes' = fromRight . hexStringToBytes

hexStringToString :: HexString -> Either String String
hexStringToString input = C.unpack <$> hexStringToBytes input

hexStringToString' :: HexString -> String
hexStringToString' = fromRight . hexStringToString

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

test_set1_challenge3 = "Cooking MC's like a pound of bacon" ~=? decodeHexXored' "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"


decodeHexXored' :: String -> String
decodeHexXored' = fromJust . decodeHexXored
  where
    fromJust (Just x) = x

decodeHexXored :: String -> Maybe String
decodeHexXored string = fst <$> decodeHexXored'' string

decodeHexXored'' :: String -> Maybe (String, Float)
decodeHexXored'' string = bestDistance
  where
    input = hexStringToBytes' string
    bestDistance :: Maybe (String, Float)
    bestDistance = if null distances then Nothing else Just (List.minimumBy (compare `on` snd) distances)
    distances :: [(String, Float)]
    distances = Either.rights $ map (\string -> (\distance -> (string, distance)) <$> frequencyDistance string) xorPossibilityStrings
    xorPossibilityStrings = map C.unpack xorPossibilities
    xorPossibilities = map (xor input . B.replicate (B.length input)) [0..255]

test_set1_challenge4 = "Now that the party is jumping\n" ~=? fst (List.minimumBy (compare `on` snd) decodedLines)
  where
    decodedLines :: [(String, Float)]
    decodedLines = mapMaybe decodeHexXored'' fileLines
    fileLines = lines fileString
    fileString = $(embedStringFile "data/4.txt")

tests = TestLabel "Lib" $ TestList
  [ test_set1_challenge1
  , test_set1_challenge2
  , test_set1_challenge3
  , test_set1_challenge4
  ]
