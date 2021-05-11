module RPM.PackageSpec (pkgspec) where

import           Test.Hspec
import           Data.Foldable(forM_)
import           Data.RPM.NV
import           Data.RPM.NVR
import           Data.RPM.Package

-- FIXME add some failures too
pkgspec :: Spec
pkgspec = do
  describe "Data.RPM.NV" $ do
    forM_ ["my-pkg-1.0.1"] $ \nv ->
      it nv $
      show (readNV nv) `shouldBe` nv

  describe "Data.RPM.NVR" $ do
    forM_ ["my-pkg-1.0.1-1.2"] $ \nvr ->
      it nvr $
      show (readNVR nvr) `shouldBe` nvr

  describe "Data.RPM.Package" $ do
    forM_ ["my-pkg-1.0.1-1.2.x86_64"] $ \pkg ->
      it pkg $
      showRpmPkg (readRpmPkg pkg) `shouldBe` pkg
