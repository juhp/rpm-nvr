-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module Data.RPM.VersionRelease (
  VersionRelease(..)
  )
where

import Data.List.Extra

import Data.RPM.VerCmp

data VersionRelease = VerRel String String
  deriving (Eq)

instance Show VersionRelease where
  show (VerRel ver rel) = ver ++ "-" ++ rel

instance Read VersionRelease where
  readsPrec _ s =
    case stripInfixEnd "-" s of
      Nothing -> error $ "readsPrec: malformed VersionRelease string " ++ s
      Just (v,r) -> [(VerRel v r, "")]

instance Ord VersionRelease where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    case rpmVerCompare v1 v2 of
      EQ -> rpmVerCompare r1 r2
      o -> o

-- appendRelease :: VersionRelease -> String -> VersionRelease
-- appendRelease (VerRel v r) d = VerRel v (r ++ d)
