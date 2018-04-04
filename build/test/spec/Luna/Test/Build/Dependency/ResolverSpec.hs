module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver

import Prologue

import Data.Text

import Test.Hspec

satData :: [(Text, Float, Float)]
satData = [("foo", 2.1, 3.5)]

spec :: Spec
spec = do
    describe "testing" $ do
        it "foo" $ resolveThings `shouldBe` (Just 1)

