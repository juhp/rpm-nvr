-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module RPM.NV (
  NV(..)
  )
where

import Data.List.Extra

data NV = NV {name :: String,
              version :: String}
        deriving (Eq)

instance Show NV where
  show (NV nm ver) = nm ++ "-" ++ ver

instance Read NV where
  readsPrec _ = readsNV

readsNV :: ReadS NV
readsNV s =
  case stripInfixEnd "-" s of
    Nothing -> error $ "readsNV: malformed NV string " ++ s
    Just (n,v) -> [(NV n v, "")]
