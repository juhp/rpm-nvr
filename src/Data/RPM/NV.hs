-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- | An NV type contains the name and version of a package only.
module Data.RPM.NV (
  NV(..),
  eitherNV,
  maybeNV
  )
where

import Data.Either.Extra
import Data.List.Extra

-- | Package name-version type
data NV = NV {name :: String,
              version :: String}
  deriving (Eq)

-- FIXME use pretty?
-- | Show instance for displaying NV (which should be Read'able)
instance Show NV where
  show (NV nm ver) = nm ++ "-" ++ ver

-- | Either read a package name-version or return error string
eitherNV :: String -> Either String NV
eitherNV s =
  case stripInfixEnd "-" s of
    Nothing -> Left $ "malformed NV string " ++ s
    Just (n,v) -> Right (NV n v)

-- | Maybe read an package name-version
maybeNV :: String -> Maybe NV
maybeNV = eitherToMaybe . eitherNV

instance Read NV where
  readsPrec _ s =
    case eitherNV s of
      Left err -> error $ "readsPrec: " ++ err ++ " " ++ s
      Right nv -> [(nv, "")]
