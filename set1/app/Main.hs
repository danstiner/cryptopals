module Main where

import Lib

main :: IO ()
main = do
  hex <- getLine
  let Right base64 = hexStringToBase64String hex
  putStrLn base64
