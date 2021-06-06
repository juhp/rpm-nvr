module RPM.PackageSpec (pkgspec) where

import           Test.Hspec
import           Data.Foldable(forM_)
import           Data.RPM.NV
import           Data.RPM.NVR
import           Data.RPM.NVRA

-- FIXME add some failures too
pkgspec :: Spec
pkgspec = do
  describe "NV" $ do
    forM_ ["my-pkg-1.0.1"] $ \nv ->
      it nv $
      showNV (readNV nv) `shouldBe` nv

  describe "NVR" $ do
    forM_ ["my-pkg-1.0.1-1.2"] $ \nvr ->
      it nvr $
      showNVR (readNVR nvr) `shouldBe` nvr

  describe "NVRA" $ do
    forM_ ["my-pkg-1.0.1-1.2.x86_64"] $ \pkg ->
      it pkg $
      showNVRA (readNVRA pkg) `shouldBe` pkg
