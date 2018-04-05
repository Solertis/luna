module Luna.Build.Dependency.Version
    ( PrereleaseType(..)
    , Prerelease(..)
    , Version(..)
    , parseVersion
    , versionP
    , prereleaseP
    , prereleaseTypeP
    ) where

import Prologue

import qualified Text.Read as R

import qualified Data.Text            as T
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P

import qualified Data.Char as C

-- TODO [Ara] Some concerns over DoS vector on large inputs

-- Versioning in Luna follows the following convention:
--      major.minor.patch-prerelease.version
--
-- The prerelease is optional.

data PrereleaseType
    = Alpha
    | Beta
    | RC
    deriving (Eq, Generic, Ord)

data Prerelease = Prerelease
    { _prType  :: !PrereleaseType
    , _version :: !Int
    } deriving (Eq, Generic, Ord)
makeLenses ''Prerelease

data Version = Version
    { _major      :: !Int
    , _minor      :: !Int
    , _patch      :: !Int
    , _prerelease :: !(Maybe Prerelease)
    } deriving (Eq, Generic)
makeLenses ''Version

instance Ord Version where
    v1@(Version maj1 min1 pat1 pre1) <= v2@(Version maj2 min2 pat2 pre2) =
        versionsOrdered && prereleaseOrdered
        where
            majorOrdered = maj1 <= maj2
            minorOrdered = min1 <= min2
            patchOrdered = pat1 <= pat2
            versionsOrdered = if majorOrdered then True
                else if minorOrdered then majorOrdered && minorOrdered
                else if patchOrdered then
                    majorOrdered && minorOrdered && patchOrdered
                else False
            prereleaseOrdered = case (pre1, pre2) of
                (Nothing, Nothing) -> True
                (Nothing, _) -> False
                (_, Nothing) -> True
                (Just (Prerelease ty1 ver1), Just (Prerelease ty2 ver2)) ->
                    if (ty1 <= ty2) then True
                    else if ver1 <= ver2 then (ty1 <= ty2) && (ver1 <= ver2)
                    else False

instance Show PrereleaseType where
    show Alpha = "alpha"
    show Beta  = "beta"
    show RC    = "rc"

instance Show Prerelease where
    show (Prerelease ty ver) = (show ty) <> "." <> (show ver)

instance Show Version where
    show (Version maj min patch pr) = nums <> (showPre pr)
        where nums    = (show maj) <> "." <> (show min) <> "." <> (show patch)
              showPre = \case
                  Nothing -> ""
                  Just pre -> "-" <> show pre

parseVersion :: Text -> Maybe Version
parseVersion tx = case P.runParser versionP "" tx of
                      Left err -> Nothing
                      Right res -> Just res

versionP :: P.Parser Version
versionP = do
    major <- natural
    minor <- P.option 0 (dot *> natural)
    patch <- P.option 0 (dot *> natural)
    prerelease <- P.optional (P.char '-' *> prereleaseP)
    if major + minor + patch == 0 then fail msg else
        pure $ Version major minor patch prerelease
    where msg = "Not all components of the version can be zero."

prereleaseP :: P.Parser Prerelease
prereleaseP = Prerelease <$> prereleaseTypeP <* dot <*> natural

prereleaseTypeP :: P.Parser PrereleaseType
prereleaseTypeP = fromString <$> p
    where fromString "alpha" = Alpha
          fromString "beta"  = Beta
          fromString "rc"    = RC
          -- `_` case not needed as it is only applied on a successful parse
          p = P.string "alpha" <|> P.string "beta" <|> P.string "rc"

dot :: P.Parser Char
dot = P.char '.'

natural :: P.Parser Int
natural = R.read <$> some P.digitChar

