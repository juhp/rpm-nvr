-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module Data.RPM.VersionRelease (
  VersionRelease(..),
  eitherVerRel,
  maybeVerRel
  )
where

import Data.Either.Extra
import Data.List.Extra

import Data.RPM.VerCmp

data VersionRelease = VerRel String String
  deriving (Eq)

instance Show VersionRelease where
  show (VerRel ver rel) = ver ++ "-" ++ rel

eitherVerRel :: String -> Either String VersionRelease
eitherVerRel s =
  case stripInfixEnd "-" s of
    Nothing -> Left $ "malformed VersionRelease string " ++ s
    Just (v,r) -> Right (VerRel v r)

maybeVerRel :: String -> Maybe VersionRelease
maybeVerRel = eitherToMaybe . eitherVerRel

instance Read VersionRelease where
  readsPrec _ s =
    case eitherVerRel s of
      Left err -> error $ "readsPrec: " ++ err ++ " " ++ s
      Right vr -> [(vr, "")]

instance Ord VersionRelease where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    case rpmVerCompare v1 v2 of
      EQ -> rpmVerCompare r1 r2
      o -> o

-- appendRelease :: VersionRelease -> String -> VersionRelease
-- appendRelease (VerRel v r) d = VerRel v (r ++ d)
