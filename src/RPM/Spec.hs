-- |
-- Module      :  RPM.Spec
-- Copyright   :  (C) 2016  Jens Petersen
--
-- Maintainer  :  Jens Petersen <petersen@fedoraproject.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: read, create, output RPM spec files

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

module RPM.Spec (
  specRead
  ) where

import Data.List

-- data Spec = Spec { preamble :: [String],
--                    fields :: [Field],
--                    description :: [String],
--                    subpackages :: [SubPackage],
--                    prep :: [String],
--                    build :: [String],
--                    install :: [String],
--                    scripts :: [String],
--                    files :: [FileList],
--                    changelog :: [ChangeEntry]}
--   deriving Show

-- --data Field = Field String String | EmptyField
-- type Field = String

-- -- data SubPackage = SubPackage
-- --                   String -- ^name
-- --                   [Field] -- ^fields
-- --                   [String] -- ^description
-- type SubPackage = String
-- --data FileList = FileList [String]
-- type FileList = String
-- --data ChangeEntry = ChangeEntry [String]
-- type ChangeEntry = String

specRead :: FilePath -> IO [[String]]
specRead f = do
  ls <- lines <$> readFile f
  return $ sections ls

sections :: [String] -> [[String]]
sections [] = []
sections ls =
  s : (sections rest)
  where
  (s, rest) = section [] ls

section :: [String] -> [String] -> ([String], [String])
section acc [] = (acc, [])
section [] (l:ls) =section [l] ls
section acc (l:ls) =
    if any (`isPrefixOf` l) sectionHead
    then (acc, l:ls)
    else section (acc ++ [l]) ls
    where
      sectionHead = ["Name:", "%description", "%package", "%prep", "%build", "%install", "%post", "%files", "%changelog"]

showSpec :: [[String]] -> String
showSpec = unlines . concat
