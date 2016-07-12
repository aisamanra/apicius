module Main where

import Parser (parseFile)

main :: IO ()
main = do
  cs <- getContents
  print (parseFile "[stdin]" cs)
