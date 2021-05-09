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

-- Todo: support RPM epochs (ENVR)

module RPM.NVR where

import Data.List (elemIndices, tails)

data NVR = NVR {name :: String,
                verrel :: VerRel}

data VerRel = VerRel {version :: String,
                      release :: String}
  deriving (Eq)

-- verrel :: VerRel -> String
-- verrel vr = version vr ++ "-" ++ release nvr

instance Eq NVR where
  (NVR n1 vr1) == (NVR n2 vr2) =
    n1 == n2 && vr1 == vr2

instance Ord VerRel where
  compare (VerRel v1 r1) (VerRel v2 r2) =
    if v1 == v2
    then compareVer r1 r2
    else compareVer v1 v2

compareVer v1 v2 =
  compare (verList v1) (verList v2)
  where
    verList :: String -> [Int]
    verList v = let (m1, rest) = break (== '.') v in
      read m1 : if null rest then [] else verList (tail rest)

instance Show VerRel where
  show (VerRel ver rel) = ver ++ "-" ++ rel

instance Show NVR where
  show (NVR nm verrel) = nm ++ "-" ++ show verrel

instance Read NVR where
  readsPrec _ = readsNVR

readsNVR :: ReadS NVR
readsNVR s =
  if length dashes < 2
    then error $ "readsNVR: malformed NVR string: '" ++ s ++ "'"
    else [(NVR (reverse eman) (VerRel (reverse rev) (reverse ler)), "")]
  where
    dashes = elemIndices '-' s

    (ler, '-':tser) = break (== '-') $ reverse s
    (rev, '-':eman) = break (== '-') tser

appendRelease :: NVR -> String -> NVR
appendRelease (NVR n vr) d =
  let rel = release vr in
  NVR n vr { release = rel ++ d}

eqNV :: NVR -> NVR -> Bool
eqNV (NVR n1 vr1) (NVR n2 vr2) =
  (n1 == n2 ) && (vr1 == vr2)
