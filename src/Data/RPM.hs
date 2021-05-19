module Data.RPM (
  NV(..),
  NVR(..),
  NVRA(..),
  VerRel(..),
  rpmVerCompare,
  dropRelease,
  addRelease
  ) where

import Data.RPM.NV
import Data.RPM.NVR
import Data.RPM.NVRA
import Data.RPM.VerCmp
--import Data.RPM.VerRel

-- | Map a name-version-release into a name-version
dropRelease :: NVR -> NV
dropRelease (NVR n (VerRel v _)) = NV n v

-- | Add a release to name-version to make an NVR
addRelease :: NV -> String -> NVR
addRelease _ "" = error "addRelease: release cannot be empty"
addRelease (NV n v) r = NVR n (VerRel v r)
