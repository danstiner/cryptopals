module Lib
    ( someFunc
    , hexStringToBase64String
    ) where

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.String

type HexString = String
type Base64String = String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

hexStringToBase64String :: HexString -> Either String Base64String
hexStringToBase64String input = bytesToBase64String <$> hexStringToBytes input

hexStringToBytes :: HexString -> Either String B.ByteString
hexStringToBytes = base16DecodeCompletely . fromString

bytesToBase64String :: B.ByteString -> Base64String
bytesToBase64String = C.unpack . Base64.encode

base16DecodeCompletely :: B.ByteString -> Either String B.ByteString
base16DecodeCompletely = toMaybe . Base16.decode
  where
    toMaybe (result, leftovers)
      | B.null leftovers = Right result
      | otherwise = Left ("Non-base16 input starting at byte " ++ show (B.length result))