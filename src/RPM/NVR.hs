-- |
-- Module      :  RPM.NVR
-- Copyright   :  (C) 2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: handling of RPM package name-version-release's (nvr's)

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module RPM.NVR where

import Data.List (elemIndices, tails)

data NVR = NVR {name :: String,
                version :: String,
                release :: String}
         deriving (Eq)

instance Ord NVR where
  compare (NVR n1 v1 r1) (NVR n2 v2 r2) =
    if n1 == n2 then compareVR (v1,r1) (v2,r2) else compare n1 n2

compareVR (v1,r1) (v2,r2) =
  if v1 == v2
  then compareVer r1 r2
  else compareVer v1 v2

compareVer v1 v2 =
  compare (verList v1) (verList v2)
  where
    verList :: String -> [Int]
    verList v = let (m1, rest) = break (== '.') v in
      read m1 : if null rest then [] else verList (tail rest)

instance Show NVR where
  show (NVR nm ver rel) = nm ++ "-" ++ ver ++ "-" ++ rel

instance Read NVR where
  readsPrec _ = readsNVR

readsNVR :: ReadS NVR
readsNVR s =
  if length (elemIndices '-' s) < 2
    then error $ "readsNVR: malformed NVR string " ++ s
    else [(NVR (reverse eman) (reverse rev) (reverse ler), "")]
  where
    (ler, '-':tser) = break (== '-') $ reverse s
    (rev, '-':eman) = break (== '-') tser
