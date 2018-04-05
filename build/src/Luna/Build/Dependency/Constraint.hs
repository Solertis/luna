module Luna.Build.Dependency.Constraint where

import Prologue hiding (Constraint)

import Luna.Build.Dependency.Version (Version)

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

type ConstraintMap = M.Map T.Text [DependencyConstraint]

type DependencyConstraint = Maybe Constraint

data ConstraintType
    = Equal
    | GT
    | LT
    | LE
    | GE
    deriving (Eq, Generic, Show)

data Constraint = Constraint
    { _conType :: !ConstraintType
    , _version :: !Version
    } deriving (Eq, Generic, Show)
makeLenses ''Constraint

