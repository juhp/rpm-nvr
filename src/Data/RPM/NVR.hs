-- |
-- Module      :  RPM.NVR
-- Copyright   :  (C) 2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: handling of RPM package name-version-release's (nvr's)

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- Todo: support RPM epochs (ENVR)

module Data.RPM.NVR (
  NVR,
  VersionRelease(..),
  appendRelease,
  eqNV
  )
where

import Data.Char
import Data.List.Extra

data NVR = NVR String VersionRelease

data VersionRelease = VerRel String String
  deriving (Eq)

-- verrel :: VerRel -> String
-- verrel vr = version vr ++ "-" ++ release nvr

instance Eq NVR where
  (NVR n1 vr1) == (NVR n2 vr2) =
    n1 == n2 && vr1 == vr2

instance Show VersionRelease where
  show (VerRel ver rel) = ver ++ "-" ++ rel

instance Show NVR where
  show (NVR nm verrel) = nm ++ "-" ++ show verrel

instance Read NVR where
  readsPrec _ s =
      case reverse (splitOn "-" s) of
        rel:ver:emaN -> [(NVR (intercalate "-" $ reverse emaN) (VerRel ver rel), "")]
        _ -> error $ "readsNVR: malformed NVR string: '" ++ s ++ "'"

appendRelease :: NVR -> String -> NVR
appendRelease (NVR n (VerRel v r)) d =
  NVR n (VerRel v (r ++ d))

eqNV :: NVR -> NVR -> Bool
eqNV (NVR n1 vr1) (NVR n2 vr2) =
  (n1 == n2 ) && (vr1 == vr2)

instance Ord VersionRelease where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    case rpmVerCompare v1 v2 of
      EQ -> rpmVerCompare r1 r2
      o -> o

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
verList = map verChunk . filter (all (/= '.')) . groupBy (\ c1 c2 -> rpmCategory c1 == rpmCategory c2)

latinChars :: [Char]
latinChars = ['A'..'Z'] ++ ['a'..'z']

rpmCategory :: Char -> RpmCharCategory
rpmCategory c | isDigit c = RpmDigit
rpmCategory c | c `elem` latinChars = RpmLatin
rpmCategory '~' = RpmTilde
rpmCategory '^' = RpmCaret
rpmCategory _ = RpmOther

-- -- eqVR True ignore release
-- eqVR :: Ignore -> VersionRelease -> VersionRelease -> Bool
-- eqVR IgnoreNone vr vr' = vr == vr'
-- eqVR IgnoreRelease (VerRel v _) (VerRel v' _) = v == v'
-- eqVR IgnoreVersion _ _ = True
