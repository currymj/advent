module Lib2 where

import Data.List.Split

initialState = (0, 0)

saturatingAdd maxMagnitude a b
  | total <= 0 = max total (-absMax)
  | total > 0 = min total absMax
    where
      total = a + b
      absMax = abs maxMagnitude

moveTransition instruction currentState
  | instruction == 'U' = addIfValidToY 1 (xcoord, ycoord)
  | instruction == 'D' = addIfValidToY (-1) (xcoord, ycoord)
  | instruction == 'L' = addIfValidToX (-1) (xcoord, ycoord)
  | instruction == 'R' = addIfValidToX 1 (xcoord, ycoord)
  where
    (xcoord, ycoord) = currentState
    isValid (x, y) = (abs x) + (abs y) <= 2
    addIfValidToY val (x, y) = let res = (x, y + val) in
      if isValid res then res else (x, y)
    addIfValidToX val (x, y) = let res = (x + val, y) in
      if isValid res then res else (x, y)

numberFromState currentState = return $ (lookupList !! zeroYCoord) !! zeroXCoord
  where
    (xcoord, ycoord) = currentState
    (zeroXCoord, zeroYCoord) = (fromIntegral $ xcoord + 2, fromIntegral $ (-ycoord) + 2)
    lookupList = [[' ', ' ', '1',' ',' '],[' ','2','3','4',' '],['5','6','7','8','9'],[' ','A','B','C',' '],[' ',' ','D',' ',' ']]


processInstructionLine initialState instructions = foldl (flip moveTransition) initialState instructions


processManyLines allLines = tail $ scanl processInstructionLine initialState allLines
