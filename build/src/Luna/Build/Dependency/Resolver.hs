module Luna.Build.Dependency.Resolver where

{- import Prologue -}

import Prelude

import Z3.Monad
import Z3.Opts

import qualified Data.Text as Text

import qualified Control.Monad.Trans as C

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Luna.Build.Dependency.Constraint

import Debug.Trace

-- Global set of resolved packages
-- `liftIO` into another monad with an IO constraint:
-- (MonadIO m) => m a

solveConstraints :: ConstraintMap -> Maybe ConstraintMap
solveConstraints = undefined

mkVersionDatatype :: Z3 Sort
mkVersionDatatype = do
    mkVersion <- mkStringSymbol "mkVersion"
    isVersion <- mkStringSymbol "isVersion"

    -- Item accessors
    getMajor <- mkStringSymbol "getMajor"
    getMinor <- mkStringSymbol "getMinor"
    getPatch <- mkStringSymbol "getPatch"
    getPre   <- mkStringSymbol "getPre"
    getPreV  <- mkStringSymbol "getPreV"

    intSort <- mkIntSort
    let recordSorter = Just intSort

    versionConst <- mkConstructor mkVersion isVersion
        [ (getMajor, recordSorter, 0)
        , (getMinor, recordSorter, 0)
        , (getPatch, recordSorter, 0)
        , (getPre,   recordSorter, 0)
        , (getPreV,  recordSorter, 0)]

    version <- mkStringSymbol "version"
    mkDatatype version [versionConst]

versionGT :: (MonadZ3 z3) => z3 AST
versionGT = undefined

versionLT :: (MonadZ3 z3) => z3 AST
versionLT = undefined

versionGE :: (MonadZ3 z3) => z3 AST
versionGE = undefined

versionLE :: (MonadZ3 z3) => z3 AST
versionLE = undefined

-- TODO [Ara] get the solution out of this
constraintScript :: Z3 (Maybe Integer)
constraintScript = do
    version <- mkVersionDatatype
    [mkVersion] <- getDatatypeSortConstructors version

    vars1 <- T.sequence $ mkInteger <$> [1, 2, 3, 2, 5] -- 1.2.3-rc.5
    t1 <- mkApp mkVersion vars1

    vars2 <- T.sequence $ mkInteger <$> [0, 0, 1, 0, 1] -- 0.0.1-alpha.1
    t2 <- mkApp mkVersion vars2

    -- package Foo == t2, >= t1 (expect unsat)

    foo <- mkFreshVar "foo" version

    assert =<< mkAnd =<< T.sequence
        [ mkEq foo t2
        , mkEq foo t1 ]

    {- check >>= C.liftIO . print -}
    check
    (result, model) <- getModel

    case result of
        Sat -> return $ Just 1
        Unsat -> return $ Nothing
        Undef -> return $ Nothing

runner :: IO (Maybe Integer)
runner = evalZ3 constraintScript >>= \mbSol ->
            case mbSol of
                Nothing  -> return Nothing
                Just sol -> return mbSol

