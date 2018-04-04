module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver

import Prologue

import Data.Text

import Test.Hspec

spec :: Spec
spec = do
    describe "testing" $ do
        it "foo" $ resolveThings `shouldBe` (Just 1)

