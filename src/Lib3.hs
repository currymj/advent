module Lib3 (
  countValidTriangles, countValidColumnTriangles) where
import Data.Maybe
import Data.List.Split

countValidTriangles :: [[Int]] -> Int
countValidTriangles inputTuples = length . filter isJust $ map triangleList inputTuples

countValidColumnTriangles :: [[Int]] -> Int
countValidColumnTriangles = length . filter isJust . generateValidColumnTriangles

generateValidColumnTriangles :: [[Int]] -> [Maybe Triangle]
generateValidColumnTriangles [al, bl, cl] = map triangleList $ concat $ map (chunksOf 3) [al, bl, cl]

data Triangle = Triangle Int Int Int
triangle a b c
  | a + b > c && b + c > a && a + c > b = Just (Triangle a b c)
  | otherwise = Nothing

triangleList :: [Int] -> Maybe Triangle
triangleList [a, b, c] = triangle a b c
