module Luna.Test.Build.Dependency.VersionSpec where

import Luna.Build.Dependency.Version

import Prologue

import Text.Megaparsec      as P
import Text.Megaparsec.Text as P

import Test.Hspec
import Test.Hspec.Megaparsec

shouldParseTo :: (Eq a, Show a) => Text -> P.Parser a -> a -> Expectation
shouldParseTo input parser result = do
    r <- mapLeft displayException <$> pure (runParser parser "" input)
    r `shouldBe` (Right result)

shouldFailToParse :: (Eq a, Show a) => Parser a -> Text -> Expectation
shouldFailToParse parser input = parse parser "" `shouldFailOn` input

spec :: Spec
spec = do
    describe "Parsing of pre-release types" $ do
        it "literal alpha" $ shouldParseTo "alpha" prereleaseTypeP Alpha
        it "literal beta"  $ shouldParseTo "beta"  prereleaseTypeP Beta
        it "literal rc"    $ shouldParseTo "rc"    prereleaseTypeP RC
        it "invalid type"  $ prereleaseTypeP `shouldFailToParse` "foo"

    describe "Parsing of pre-release tags" $ do
        it "alpha tag"         $ shouldParseTo "alpha.1" prereleaseP
                                 (Prerelease Alpha 1)
        it "beta tag"          $ shouldParseTo "beta.231" prereleaseP
                                 (Prerelease Beta 231)
        it "rc tag"            $ shouldParseTo "rc.0" prereleaseP
                                 (Prerelease RC 0)
        it "silly number"      $ shouldParseTo "alpha.0000" prereleaseP
                                 (Prerelease Alpha 0)
        it "invalid tag"       $ prereleaseP `shouldFailToParse` "foo.1"
        it "missing number"    $ prereleaseP `shouldFailToParse` "alpha."
        it "letter for number" $ prereleaseP `shouldFailToParse` "alpha.a"

    describe "Parsing of version strings" $ do
        it "basic version string" $
            shouldParseTo "0.0.1-alpha.0"
            versionP (Version 0 0 1 (Just (Prerelease Alpha 0)))
        it "omission of patch" $
            shouldParseTo "1.0" versionP (Version 1 0 0 Nothing)
        it "omission of patch with prerelease" $
            shouldParseTo "12.2-beta.1"
            versionP (Version 12 2 0 (Just (Prerelease Beta 1)))
        it "omission of minor version" $
            shouldParseTo "12" versionP (Version 12 0 0 Nothing)
        it "omission of minor version with prerelease" $
            shouldParseTo "12-rc.2" versionP
            (Version 12 0 0 (Just (Prerelease RC 2)))
        it "omission of prerelease" $
            shouldParseTo "2.3.1" versionP (Version 2 3 1 Nothing)
        it "invalid version" $ versionP `shouldFailToParse` "0.0.0"
        it "invalid version with prerelease" $
            versionP `shouldFailToParse` "0.0.0-alpha.1"

    describe "Version ordering" $ do
        it "correctly ordered versions" $ (Version 0 0 1 Nothing)
            `shouldSatisfy` (< (Version 1 1 2 Nothing))
        it "incorrectly ordered versions" $ (Version 1 1 2 Nothing)
            `shouldNotSatisfy` (< (Version 0 2 1 Nothing))
        it "ordering of prerelease vs no prerelease" $
            (Version 1 2 1 Nothing) `shouldSatisfy`
            (> (Version 1 2 1 (Just (Prerelease Alpha 1))))
        it "ordering within prereleases, same prerelease stage" $
            (Version 1 1 1 (Just (Prerelease Alpha 1))) `shouldSatisfy`
            (< (Version 1 1 1 (Just (Prerelease Alpha 2))))
        it "ordering within prereleases, different prerelease stage" $
            (Version 1 1 1 (Just (Prerelease Alpha 1))) `shouldSatisfy`
            (< (Version 1 1 1 (Just (Prerelease Beta 1))))
        it "equal versions" $ (Version 1 1 1 Nothing) `shouldSatisfy`
            (== (Version 1 1 1 Nothing))
        it "non-equal versions" $ (Version 1 1 1 Nothing) `shouldNotSatisfy`
            (== (Version 1 1 1 (Just (Prerelease Alpha 1))))
        it "foo" $ (Version 1 7 13 Nothing) `shouldSatisfy` (< (Version 84 37 52 Nothing))
        it "bar" $ (Version 44 31 36 Nothing) `shouldSatisfy` (< (Version 71 46 3 (Just (Prerelease Beta 7))))

