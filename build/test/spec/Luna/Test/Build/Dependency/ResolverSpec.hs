module Luna.Test.Build.Dependency.ResolverSpec where

import Luna.Build.Dependency.Resolver
import Luna.Build.Dependency.Version
import Luna.Build.Dependency.Constraint

import Prologue

import qualified Data.Map.Strict as M (fromList)

import Test.Hspec

shouldSolve :: ConstraintMap -> Expectation
shouldSolve = undefined

shouldNotSolve :: ConstraintMap -> Expectation
shouldNotSolve = undefined

shouldSolveAs :: ConstraintMap -> ConstraintMap -> Expectation
shouldSolveAs = undefined

testConstraints :: ConstraintMap
testConstraints = M.fromList [("foo", [])]

spec :: Spec
spec = do
    describe "testing" $ do
        it "foo" $ do
            result <- solveConstraints testConstraints
            result `shouldBe` (Just 1)

