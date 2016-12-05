module Lib4 where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Control.Monad
import Data.Tuple

lettersDash :: Parser [Char]
lettersDash = liftM concat $ sepEndBy (many1 letter) (char '-')

sectorId = integer

checkSum :: Parser [Char]
checkSum = between (char '[') (char ']') (many1 letter)

allParse :: Parser ([Char], Int, [Char])
allParse = (,,) <$> lettersDash <*> liftM read (many1 digit) <*> checkSum
