module Luna.Build.Dependency.Constraint where

import Prologue hiding (Constraint)

import Luna.Build.Dependency.Version (Version(..), version)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Text as P

type ConstraintMap = M.Map T.Text [Constraint]

data ConstraintType
    = ConstraintEQ
    | ConstraintGT
    | ConstraintLT
    | ConstraintLE
    | ConstraintGE
    deriving (Eq, Generic, Ord, Show)

data Constraint = Constraint
    { _conType :: !ConstraintType
    , _version :: !Version
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Constraint

-----------------------
-- Parsing Functions --
-----------------------

-- foo (>= 2.1.3 && <= 3.0.0) || 2.0.8

-- TODO [Ara] Check ranges are proper before converting to constraints

constraintsP :: P.Parser [Constraint]
constraintsP = undefined

constraintP :: P.Parser Constraint
constraintP = flip Constraint (Version 1 1 1 Nothing) <$> operatorP <* spaces
{- constraintP = Constraint <$> operatorP <* spaces <*> versionP <* spaces -}

operatorP :: P.Parser ConstraintType
operatorP = P.choice
    [ ConstraintEQ <$ P.string "=="
    , ConstraintLE <$ P.string "<="
    , ConstraintGE <$ P.string ">="
    , ConstraintGT <$ P.string ">"
    , ConstraintLT <$ P.string "<" ]

and :: P.Parser ()
and = spaces *> P.string "&&" *> spaces >> pure ()

spaces :: P.Parser ()
spaces = many P.space >> pure ()

