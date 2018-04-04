module Luna.Build.Dependency.Constraint where

import Prologue

import qualified Data.Map.Strict as M
import qualified Data.Text       as T

data DependencyConstraint = Unconstrained
                | Equal Double
                | GT Double
                | LT Double
                | LE Double
                | GE Double
                deriving (Eq, Generic, Show)

makeLenses ''DependencyConstraint

type ConstraintMap = M.Map T.Text [DependencyConstraint]

