module Luna.Build.Dependency.Resolver where

import Prologue

import qualified Prelude as P

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

mkVersionDatatype :: (MonadZ3 m) => m Sort
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

mkVersionLT :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionLT l r = do
    version <- mkVersionDatatype
    [[getMajor, getMinor, getPatch, getPre, getPreV]] <-
        getDatatypeSortConstructorAccessors version

    -- Get AST nodes corresponding to record fields for l and r
    majorL <- mkApp getMajor [l]
    majorR <- mkApp getMajor [r]
    minorL <- mkApp getMinor [l]
    minorR <- mkApp getMinor [r]
    patchL <- mkApp getPatch [l]
    patchR <- mkApp getPatch [r]
    preL   <- mkApp getPre   [l]
    preR   <- mkApp getPre   [r]
    preVL  <- mkApp getPreV  [l]
    preVR  <- mkApp getPreV  [r]

    -- Utility nodes
    true  <- mkTrue
    false <- mkFalse

    -- The following Z3 formula implements this logic
    --
    -- if majorL < majorR then True
    -- else if majorL == majorR then
    --     if minorL < minorR then True
    --     else if minorL == minorR then
    --         if patchL < patchR then True
    --         else if patchL == patchR then
    --             if preL < preR then True
    --             else if preL == preR then
    --                 if preVL < preVR then True
    --                 else False
    --             else False
    --         else False
    --     else False
    -- else False

    -- Build the condition components for LT expr
    majorLT <- mkLt majorL majorR
    majorEQ <- mkEq majorL majorR
    minorLT <- mkLt minorL minorR
    minorEQ <- mkEq minorL minorR
    patchLT <- mkLt patchL patchR
    patchEQ <- mkEq patchL patchR
    preLT   <- mkLt preL preR
    preEQ   <- mkEq preL preR
    preVLT  <- mkLt preVL preVR

    -- Build the sub if-then-else expressions
    ifPreVLT  <- mkIte preVLT true false
    ifPreEQ   <- mkIte preEQ preVLT false
    ifPreLT   <- mkIte preLT true ifPreEQ
    ifPatchEQ <- mkIte patchEQ ifPreLT false
    ifPatchLT <- mkIte patchLT true ifPatchEQ
    ifMinorEQ <- mkIte minorEQ ifPatchLT false
    ifMinorLT <- mkIte minorLT true ifMinorEQ
    ifMajorEQ <- mkIte majorEQ ifMinorLT false

    -- Evaluate the expression
    mkIte majorLT true ifMajorEQ

mkVersionLE :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionLE l r = do
    lt <- mkVersionLT l r
    eq <- mkEq l r
    mkOr [lt, eq]

mkVersionGT :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionGT l r = do
    le <- mkVersionLE l r
    mkNot le

mkVersionGE :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionGE l r = do
    lt <- mkVersionLT l r
    mkNot lt

-- TODO [Ara] get the solution out of this
constraintScript :: Z3 (Maybe Integer)
constraintScript = do
    version <- mkVersionDatatype

    -- Only a single constructor with accessors
    [mkVersion] <- getDatatypeSortConstructors version
    [[getMajor, getMinor, getPatch, getPre, getPreV]] <-
        getDatatypeSortConstructorAccessors version

    vars1 <- T.sequence $ mkInteger <$> [44, 31, 36, 3, 0] -- 1.2.3-rc.5
    t1 <- mkApp mkVersion vars1

    vars2 <- T.sequence $ mkInteger <$> [71, 46, 3, 1, 7] -- 0.0.1-alpha.1
    t2 <- mkApp mkVersion vars2

    -- package Foo == t2, >= t1 (expect unsat)

    foo <- mkFreshVar "foo" version

    major1 <- mkApp getMajor [t1]
    major2 <- mkApp getMajor [t2]

    Z3.Monad.assert =<< mkAnd =<< T.sequence
        [ mkVersionLT t1 t2 ]
        {- [ mkVersionLT foo t1 -}
        {- , mkEq foo t2 ] -}

    {- check >>= C.liftIO . print -}
    check
    (result, model) <- getModel

    case result of
        Sat -> pure $ Just 1
        Unsat -> pure $ Nothing
        Undef -> pure $ Nothing

runner :: IO (Maybe Integer)
runner = evalZ3 constraintScript >>= \mbSol ->
            case mbSol of
                Nothing  -> pure Nothing
                Just sol -> pure mbSol

