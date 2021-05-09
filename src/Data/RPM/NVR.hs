-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module Data.RPM.NVR (
  NVR,
  VersionRelease(..),
  appendRelease,
--  dropRelease
  )
where

import Data.List.Extra

import Data.RPM.VerCmp

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

instance Ord VersionRelease where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    case rpmVerCompare v1 v2 of
      EQ -> rpmVerCompare r1 r2
      o -> o

appendRelease :: NVR -> String -> NVR
appendRelease (NVR n (VerRel v r)) d =
  NVR n (VerRel v (r ++ d))

-- dropRelease :: NVR -> NV
-- dropRelease (NVR n v _) = NV n v
