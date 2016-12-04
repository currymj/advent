module Main where

import Lib1
import Lib2
import Data.List.Split


advent2 = readFile "src/inputFile2.txt" >>=
  (putStrLn . concat . map numberFromState . processManyLines . init . splitOn "\n")

main :: IO ()
main = advent2
