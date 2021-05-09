{-# LANGUAGE CPP #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module Data.RPM.Package (
  RpmPackage(..),
  readRpmPkg,
  showRpmPkg,
  dropRpmArch,
  showPkgIdent,
  showPkgVerRel,
  archSuffix
  )
where

import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR

-- FIXME: add epoch type
-- | RPM package with name, version-release, and maybe architecture
data RpmPackage = RpmPkg {rpmName :: String,
                          rpmVerRel :: VersionRelease,
                          rpmMArch :: Maybe String}
  deriving (Eq, Ord)

-- | Render an RpmPackage
showRpmPkg :: RpmPackage -> String
showRpmPkg (RpmPkg n vr ma) = n <> "-" <> show vr <> "." <> fromMaybe "" ma

-- | Parse an RpmPackage with arch suffix
readRpmPkg :: String -> RpmPackage
readRpmPkg t =
  case reverse pieces of
    rel:ver:emaN -> RpmPkg (intercalate "-" $ reverse emaN) (VerRel ver rel) (Just arch)
    _ -> error $ "Malformed rpm package name: " ++ t
  where
    -- FIXME what if no arch suffix
    (nvr',arch) = breakOnEnd "." $ fromMaybe t $ stripSuffix ".rpm" t
    pieces = splitOn "-" $ dropEnd 1 nvr'

-- | Show the version-release of an RpmPackage
showPkgVerRel :: RpmPackage -> String
showPkgVerRel = show . rpmVerRel

-- | Identifier for an RPM package identified by name and arch
showPkgIdent :: RpmPackage -> String
showPkgIdent p = rpmName p <> archSuffix p

-- | Helper to add an arch suffix
archSuffix :: RpmPackage -> String
archSuffix p = maybe "" ("." <>) (rpmMArch p)

-- | drop arch from RpmPackage
dropRpmArch :: RpmPackage -> RpmPackage
dropRpmArch (RpmPkg n vr _) = RpmPkg n vr Nothing
