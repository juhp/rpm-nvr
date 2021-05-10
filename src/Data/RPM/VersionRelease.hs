-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | A type for carrying the version and release of an rpm package.
module Data.RPM.VersionRelease (
  VersionRelease(..),
  eitherVerRel,
  maybeVerRel
  )
where

import Data.Either.Extra
import Data.List.Extra

import Data.RPM.VerCmp

-- | The version-release of an (rpm) package
--
-- FIXME: use rpmVerCompare for equality like codec-rpm, instead of derived Eq
data VersionRelease = VerRel String String
  deriving (Eq)

instance Show VersionRelease where
  show (VerRel ver rel) = ver ++ "-" ++ rel

-- | Either read a package version-release or return an failure string
eitherVerRel :: String -> Either String VersionRelease
eitherVerRel s =
  case stripInfixEnd "-" s of
    Nothing -> Left $ "malformed VersionRelease string " ++ s
    Just (v,r) -> Right (VerRel v r)

-- | Maybe read a package version-release
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
