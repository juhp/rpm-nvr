-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module Data.RPM.VerCmp (rpmVerCompare)
where

import Data.Char (isDigit)
import Data.List (groupBy)

data VerChunk = TildeChunk Int | CaretChunk Int | TxtChunk String | IntChunk Int
  deriving (Eq,Ord,Show)

data RpmCharCategory = RpmTilde | RpmCaret | RpmOther | RpmLatin | RpmDigit
  deriving Eq

verChunk :: String -> VerChunk
verChunk t | all isDigit t = (IntChunk . read) t
verChunk t | all (== '~') t = (TildeChunk . length) t
verChunk t | all (== '^') t = (CaretChunk . length) t
verChunk t = TxtChunk t

rpmVerCompare :: String -> String -> Ordering
rpmVerCompare v1 v2 | v1 == v2 = EQ
rpmVerCompare v1 v2 =
  compareChunks (verList v1) (verList v2)
  where
    compareChunks [] [] = EQ
    compareChunks (c:cs) (c':cs') | c == c' = compareChunks cs cs'
    compareChunks ((TildeChunk n):_) ((TildeChunk n'):_) = compare n' n
    compareChunks ((CaretChunk n):_) ((CaretChunk n'):_) = compare n' n
    compareChunks ((TildeChunk _):_) _ = LT
    compareChunks _ ((TildeChunk _):_) = GT
    compareChunks ((CaretChunk _):_) _ = LT
    compareChunks _ ((CaretChunk _):_) = GT
    compareChunks _ [] = GT
    compareChunks [] _ = LT
    compareChunks (c:_) (c':_) = compare c c'

verList :: String -> [VerChunk]
verList = map verChunk . filter (notElem '.') . groupBy (\ c1 c2 -> rpmCategory c1 == rpmCategory c2)

latinChars :: [Char]
latinChars = ['A'..'Z'] ++ ['a'..'z']

rpmCategory :: Char -> RpmCharCategory
rpmCategory c | isDigit c = RpmDigit
rpmCategory c | c `elem` latinChars = RpmLatin
rpmCategory '~' = RpmTilde
rpmCategory '^' = RpmCaret
rpmCategory _ = RpmOther
