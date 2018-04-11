module Luna.Build.Dependency.Resolver ( solveConstraints ) where

import Prologue

import qualified Prelude as P

import qualified Data.Text as Text

import qualified Control.Monad.Trans as C
import qualified Data.Map.Strict     as M
import qualified Data.Traversable    as T

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe

import Luna.Build.Dependency.Constraint

import Debug.Trace

solveConstraints :: (MonadIO m) => ConstraintMap -> m (Maybe Int)
solveConstraints constraints = pure $ Just 1

