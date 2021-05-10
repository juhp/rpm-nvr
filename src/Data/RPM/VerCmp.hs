{-# LANGUAGE CPP, MultiWayIf #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- String-ified version of vercmp from codec-rpm-0.2.2/Codec/RPM/Version.hs
-- Copyright 2016-2018 Red Hat
-- Copyright 2021 Jens Petersen

-- | Compare versions or releases using rpm's vercmp algorithm
module Data.RPM.VerCmp (rpmVerCompare)
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List ({-groupBy,-} isPrefixOf)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- data VerChunk = TildeChunk Int | CaretChunk Int | TxtChunk String | IntChunk Int
--   deriving (Eq,Ord,Show)

-- data RpmCharCategory = RpmTilde | RpmCaret | RpmOther | RpmLatin | RpmDigit
--   deriving Eq

-- verChunk :: String -> VerChunk
-- verChunk t | all isDigit t = (IntChunk . read) t
-- verChunk t | all (== '~') t = (TildeChunk . length) t
-- verChunk t | all (== '^') t = (CaretChunk . length) t
-- verChunk t = TxtChunk t


-- -- my original version fails the tests for some symbols like _ and +
-- rpmVerCompare :: String -> String -> Ordering
-- rpmVerCompare v1 v2 | v1 == v2 = EQ
-- rpmVerCompare v1 v2 =
--   compareChunks (verList v1) (verList v2)
--   where
--     compareChunks [] [] = EQ
--     compareChunks (c:cs) (c':cs') | c == c' = compareChunks cs cs'
--     compareChunks ((TildeChunk n):_) ((TildeChunk n'):_) = compare n' n
--     compareChunks ((CaretChunk n):_) ((CaretChunk n'):_) = compare n' n
--     compareChunks ((TildeChunk _):_) _ = LT
--     compareChunks _ ((TildeChunk _):_) = GT
--     compareChunks ((CaretChunk _):_) _ = LT
--     compareChunks _ ((CaretChunk _):_) = GT
--     compareChunks _ [] = GT
--     compareChunks [] _ = LT
--     compareChunks (c:_) (c':_) = compare c c'

-- verList :: String -> [VerChunk]
-- verList = map verChunk . filter (notElem '.') . groupBy (\ c1 c2 -> rpmCategory c1 == rpmCategory c2)

-- latinChars :: [Char]
-- latinChars = ['A'..'Z'] ++ ['a'..'z']

-- rpmCategory :: Char -> RpmCharCategory
-- rpmCategory c | isDigit c = RpmDigit
-- rpmCategory c | c `elem` latinChars = RpmLatin
-- rpmCategory '~' = RpmTilde
-- rpmCategory '^' = RpmCaret
-- rpmCategory _ = RpmOther

-- | Compare two version numbers and return an 'Ordering'.
--
-- Native implementation of rpm's C vercmp
rpmVerCompare :: String -> String -> Ordering
rpmVerCompare a b =
  if a == b then EQ
    else let
    -- strip out all non-version characters
    -- keep in mind the strings may be empty after this
    a' = dropSeparators a
    b' = dropSeparators b

    -- rpm compares strings by digit and non-digit components, so grab the first
    -- component of one type
    fn = if isDigit (head a') then isDigit else isAsciiAlpha
    (prefixA, suffixA) = span fn a'
    (prefixB, suffixB) = span fn b'
 in
    if | a' == b'                                       -> EQ
       -- Nothing left means the versions are equal
       {- null a' && null b'                             -> EQ -}
       -- tilde is less than everything, including an empty string
       | ("~" `isPrefixOf` a') && ("~" `isPrefixOf` b') -> rpmVerCompare (tail a') (tail b')
       | ("~" `isPrefixOf` a')                            -> LT
       | ("~" `isPrefixOf` b')                            -> GT
       -- caret is more than everything, except .
       | ("^" `isPrefixOf` a') && ("^" `isPrefixOf` b') -> rpmVerCompare (tail a') (tail b')
       | ("^" `isPrefixOf` a') && null b'               -> GT
       | null a' && ("^" `isPrefixOf` b')               -> LT
       | ("^" `isPrefixOf` a')                            -> LT
       | ("^" `isPrefixOf` b')                            -> GT
       -- otherwise, if one of the strings is null, the other is greater
       | (null a')                                        -> LT
       | (null b')                                        -> GT
       -- Now we have two non-null strings, starting with a non-tilde version character
       -- If one prefix is a number and the other is a string, the one that is a number
       -- is greater.
       | isDigit (head a') && (not . isDigit) (head b') -> GT
       | (not . isDigit) (head a') && isDigit (head b') -> LT
       | isDigit (head a')                                -> (prefixA `compareAsInts` prefixB) <> (suffixA `rpmVerCompare` suffixB)
       | otherwise                                          -> (prefixA `compare` prefixB) <> (suffixA `rpmVerCompare` suffixB)
 where
    compareAsInts :: String -> String -> Ordering
    -- the version numbers can overflow Int, so strip leading 0's and do a string compare,
    -- longest string wins
    compareAsInts x y =
        let x' = dropWhile (== '0') x
            y' = dropWhile (== '0') y
        in
            if length x' > length y' then GT
            else x' `compare` y'

    -- isAlpha returns any unicode alpha, but we just want ASCII characters
    isAsciiAlpha :: Char -> Bool
    isAsciiAlpha x = isAsciiLower x || isAsciiUpper x

    -- RPM only cares about ascii digits, ascii alpha, and ~ ^
    isVersionChar :: Char -> Bool
    isVersionChar x = isDigit x || isAsciiAlpha x || x == '~' || x == '^'

    dropSeparators :: String -> String
    dropSeparators = dropWhile (not . isVersionChar)
