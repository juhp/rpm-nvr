-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module Data.RPM.NVR (
  NVR,
  VersionRelease(..),
--  appendRelease,
--  dropRelease
  )
where

import Data.List.Extra

import Data.RPM.VersionRelease

-- FIXME should take vercmp equality into account?
data NVR = NVR String VersionRelease
  deriving Eq

instance Show NVR where
  show (NVR nm verrel) = nm ++ "-" ++ show verrel

instance Read NVR where
  readsPrec _ s =
      case reverse (splitOn "-" s) of
        rel:ver:emaN -> [(NVR (intercalate "-" $ reverse emaN) (VerRel ver rel), "")]
        _ -> error $ "readsNVR: malformed NVR string: '" ++ s ++ "'"

-- appendRelease :: NVR -> String -> NVR
-- appendRelease (NVR n (VerRel v r)) d =
--   NVR n (VerRel v (r ++ d))

-- dropRelease :: NVR -> NV
-- dropRelease (NVR n v _) = NV n v
