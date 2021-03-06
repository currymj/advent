module Main where

import Lib1
import Lib2
import Data.List.Split
import Data.List (transpose)
import Lib3
import Lib4
import Text.Printf

advent2 = readFile "src/inputFile2.txt" >>=
  (putStrLn . concat . map numberFromState . processManyLines . init . splitOn "\n")

splitStringIntoTuple :: String -> [Int]
splitStringIntoTuple s = map read $ words s

advent3 = readFile "src/inputFile3.txt" >>=
  --(putStrLn . show . countValidTriangles . map splitStringIntoTuple . init . splitOn "\n")
  (putStrLn . show . countValidColumnTriangles . transpose . map splitStringIntoTuple . init . splitOn "\n")
advent4 = readFile "src/rooms.txt" >>=
  --(putStrLn . show . countComputedIds . init . splitOn "\n")
  (putStrLn . concat . map (uncurry (printf "%s %d\n")) . getRotValid . init . splitOn "\n")

main :: IO ()
main = advent4
