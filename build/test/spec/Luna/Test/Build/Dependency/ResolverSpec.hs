module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver

import Prologue

import Test.Hspec

spec :: Spec
spec = do
    describe "testing" $ do
        it "foo" $ do
            result <- runner
            result `shouldBe` (Just 1)

