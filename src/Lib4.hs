module Lib4 (countComputedIds, getRotValid) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Control.Monad
import Data.Maybe
import Data.Tuple
import Data.List
import Data.Ord
import Data.Either

lettersDash :: Parser [Char]
lettersDash = liftM concat $ sepEndBy (many1 letter) (char '-')

sectorId = integer

checkSum :: Parser [Char]
checkSum = between (char '[') (char ']') (many1 letter)


myTupleCompare (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2
allParse :: Parser ([Char], Int, [Char])
allParse = (,,) <$> lettersDash <*> liftM read (many1 digit) <*> checkSum

computeCheckSum chList = concat . take 5 . map snd $ sortBy myTupleCompare $ map (\x -> (length x, take 1 x)) $ group $ sort chList
processParsed (Right (ch, num, check)) = if computeCheckSum ch == check then Just num else Nothing
processParsed (Left _) = Nothing

countComputedIds = sum . map (maybe 0 id) . map (\x -> processParsed $ parse allParse x x)


rotChar :: Int -> Char -> Char
rotChar n ch = toEnum $ (97+) $ mod ((fromEnum ch) - 97 + n) 26
rot n = map (rotChar n)

processRot (Right (ch, num, check)) = if computeCheckSum ch == check then Just (rot num ch, num) else Nothing
processRot (Left _) = Nothing

getRotValid = map (maybe ("", 0) id) . filter isJust . map (processRot . parse allParse "")
