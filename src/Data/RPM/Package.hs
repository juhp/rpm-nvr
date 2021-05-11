{-# LANGUAGE CPP #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | A type for a (binary) RPM package.
module Data.RPM.Package (
  RpmPackage(..),
  readRpmPkg,
  eitherRpmPkg,
  maybeRpmPkg,
  showRpmPkg,
  dropRpmArch,
  showPkgIdent,
  showPkgVerRel,
  archSuffix
  )
where

import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR

-- | RPM package with name, version-release, and maybe architecture
--
-- If arch is not needed use NVR instead.
--
-- FIXME: add epoch field
data RpmPackage = RpmPkg {rpmName :: String,
                          rpmVerRel :: VersionRelease,
                          rpmMArch :: Maybe String}
  deriving (Eq, Ord)

-- | Render an RpmPackage
showRpmPkg :: RpmPackage -> String
showRpmPkg (RpmPkg n vr ma) = n <> "-" <> show vr <> "." <> fromMaybe "" ma

-- | Either read a name-version-release.arch or return a failure string
eitherRpmPkg :: String -> Either String RpmPackage
eitherRpmPkg "" = Left "RpmPackage string cannot be empty"
eitherRpmPkg s@('-':_) = Left $ "RpmPackage cannot start with '-': " ++ s
eitherRpmPkg s =
  case reverse pieces of
    ps@(rel:ver:emaN) ->
      if any null (arch:ps)
      then Left $ "Bad RpmPackage string: " ++ s
      else Right $ RpmPkg (intercalate "-" $ reverse emaN) (VerRel ver rel) (Just arch)
    _ -> Left $ "RpmPackage string must have form 'name-version-release.arch': " ++ s
  where
    -- FIXME what if no arch suffix
    (nvr',arch) = breakOnEnd "." $ fromMaybe s $ stripSuffix ".rpm" s
    pieces = splitOn "-" $ dropEnd 1 nvr'

-- | Maybe read an RpmPackage
maybeRpmPkg :: String -> Maybe RpmPackage
maybeRpmPkg = eitherToMaybe . eitherRpmPkg

-- | Parse an RpmPackage with arch suffix
--
-- Errors if not of the form "name-version-release[.arch]"
readRpmPkg :: String -> RpmPackage
readRpmPkg = either error id . eitherRpmPkg

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
