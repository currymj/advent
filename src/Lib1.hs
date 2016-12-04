module Lib1
    ( totalRepeatDistanceFromInputString 
    ) where
import Data.List.Split
import Data.List
initialTuple = (0, 0, 0)

getInstructionsFromString (turnDirection:remainingString) =
  let turnInstruction = if turnDirection == 'R' then 1 else -1
      walkDirection = read remainingString :: Integer
      in
    (turnInstruction, walkDirection)

getSign x | x > 0 = 1
          | x <= 0 = -1
updateTuple instruction directionState =
  let xIncrement | newDirection == 1 = (walkDistance)
                 | newDirection == 3 = (-walkDistance)
                 | otherwise = 0
      yIncrement | newDirection == 0 = (walkDistance)
                 | newDirection == 2 = (-walkDistance)
                 | otherwise = 0
  in tail [(newDirection, xcoord + i, ycoord + j) |
      i <- [0,(getSign xIncrement)..xIncrement],
      j <- [0,(getSign yIncrement)..yIncrement]]
  where
    (direction, xcoord, ycoord) = last directionState
    (turnInstruction, walkDistance) = instruction
    newDirection = (direction + turnInstruction) `mod` 4

processInString = concat . scanl (flip (updateTuple . getInstructionsFromString)) [initialTuple] . (splitOn ", ")

reversedHistoryLists = tail . scanl (flip (:)) []

firstIsDuplicate (x:xs) = elem x xs

findDuplicates = filter firstIsDuplicate . reversedHistoryLists . coordsOnly

coordsOnly = (map (\(_,x,y) -> (x,y)))

addTuple (x, y) = abs x + abs y
totalRepeatDistanceFromInputString = addTuple . (head . head) . findDuplicates . processInString
