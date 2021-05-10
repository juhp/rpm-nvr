-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | A type for name-version-release of an RPM package
module Data.RPM.NVR (
  NVR(..),
  eitherNVR,
  maybeNVR,
  VersionRelease(..),
--  appendRelease,
  dropRelease
  )
where

import Data.Either.Extra
import Data.List.Extra

import Data.RPM.NV
import Data.RPM.VersionRelease

-- | An rpm package name-version-release
data NVR = NVR String VersionRelease
  deriving Eq

instance Show NVR where
  show (NVR nm verrel) = nm ++ "-" ++ show verrel

-- | Either read a package name-version-release or return a failure string
eitherNVR :: String -> Either String NVR
eitherNVR s =
  case reverse (splitOn "-" s) of
    rel:ver:emaN -> Right (NVR (intercalate "-" $ reverse emaN) (VerRel ver rel))
    _ -> Left $ "malformed NVR string: '" ++ s ++ "'"

-- | Maybe read a package name-version-release string
maybeNVR :: String -> Maybe NVR
maybeNVR = eitherToMaybe . eitherNVR

instance Read NVR where
  readsPrec _ s =
    case eitherNVR s of
      Left err -> error $ "readsPrec: " ++ err ++ " " ++ s
      Right nvr -> [(nvr, "")]

-- -- FIXME include "." or not?
-- appendRelease :: NVR -> String -> NVR
-- appendRelease (NVR n (VerRel v r)) d =
--   NVR n (VerRel v (r ++ d))

-- | Map a name-version-release into a name-version
dropRelease :: NVR -> NV
dropRelease (NVR n (VerRel v _)) = NV n v
