-- |
-- Module      :  RPM.NV
-- Copyright   :  (C) 2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: handling of RPM package name-versions

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module RPM.NV where

data NV = NV {name :: String,
              version :: String}
        deriving (Eq)

instance Show NV where
  show (NV nm ver) = nm ++ "-" ++ ver

instance Read NV where
  readsPrec _ = readsNV

readsNV :: ReadS NV
readsNV s =
  if '-' `notElem` s
    then error $ "readsNV: malformed NV string " ++ s
    else [(NV (reverse eman) (reverse rev), "")]
  where
    (rev, '-':eman) = break (== '-') $ reverse s
