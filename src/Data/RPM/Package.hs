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
  showPkgArch
  )
where

import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.RPM.NVR

-- | RPM package with name, version-release, and maybe architecture
data RpmPackage = RpmPkg {rpmName :: String,
                          rpmVerRel :: VersionRelease,
                          rpmMArch :: Maybe String}
  deriving (Eq, Ord)

-- | Show the version-release of an RpmPackage
showPkgVerRel :: RpmPackage -> String
showPkgVerRel = txtVerRel . rpmVerRel
  where
    txtVerRel (VerRel v r) = v <> "-" <> r

-- | Identifier for an RPM package identified by name and arch
showPkgIdent :: RpmPackage -> String
showPkgIdent p = rpmName p <> showPkgArch p

-- | Helper to add an arch suffix
showPkgArch :: RpmPackage -> String
showPkgArch p = maybe "" ("." <>) (rpmMArch p)

-- | drop arch from RpmPackage
dropRpmArch :: RpmPackage -> RpmPackage
dropRpmArch (RpmPkg n vr _) = RpmPkg n vr Nothing

-- | Render an RpmPackage
showRpmPkg :: RpmPackage -> String
showRpmPkg p = showPkgIdent p <> "  " <> showPkgVerRel p

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
