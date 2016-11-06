module Main where

import Lib
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

input = hexStringToBytes' "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
xorPossibilityStrings = map C.unpack xorPossibilities
xorPossibilities = map (xor input . C.replicate (B.length input)) ['0'..'z']

main :: IO ()
main = mapM_ putStrLn xorPossibilityStrings
