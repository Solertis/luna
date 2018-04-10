module Luna.Build.Dependency.Resolver ( solveConstraints ) where

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
import qualified Data.Map.Strict  as M

import Luna.Build.Dependency.Constraint

import Debug.Trace

-- Global set of resolved packages
-- `liftIO` into another monad with an IO constraint:
-- (MonadIO m) => m a

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
mkVersionGT l r = mkVersionLE l r >>= mkNot

mkVersionGE :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionGE l r = mkVersionLT l r >>= mkNot

mkVersionEQ :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionEQ l r = mkEq l r

mkVersionNE :: (MonadZ3 m) => AST -> AST -> m AST
mkVersionNE l r = mkEq l r >>= mkNot

-- TODO [Ara] get the solution out of this
constraintScript :: ConstraintMap -> Z3 (Maybe Model)
constraintScript constraints = do
    version <- mkVersionDatatype

    -- Only a single constructor with accessors
    [mkVersion] <- getDatatypeSortConstructors version
    let mkVersionPure = pure mkVersion :: Z3 FuncDecl
    [[getMajor, getMinor, getPatch, getPre, getPreV]] <-
        getDatatypeSortConstructorAccessors version

    -- Get the constraints
    let packageConstraints = M.elems constraints
        packageNames = Text.unpack <$> M.keys constraints

    traceShowM packageConstraints

    varNames <- T.sequence $ (flip mkFreshVar version) . Text.unpack
                          <$> M.keys constraints

    -- Consider the following package set
    -- foo <= 1.3.1
    -- foo = 1.0.0-beta.3
    -- bar > 1.3.0
    -- baz = 2.0.0-rc.1
    -- baz >= 1.3.2

    vars1 <- T.sequence $ mkInteger <$> [1, 3, 1, 3, 0]
    v1 <- mkApp mkVersion vars1

    vars2 <- T.sequence $ mkInteger <$> [2, 0, 0, 1, 3]
    v2 <- mkApp mkVersion vars2

    vars3 <- T.sequence $ mkInteger <$> [1, 3, 0, 3, 0]
    v3 <- mkApp mkVersion vars3

    vars4 <- T.sequence $ mkInteger <$> [2, 0, 0, 2, 1]
    v4 <- mkApp mkVersion vars4

    vars5 <- T.sequence $ mkInteger <$> [1, 3, 2, 3, 0]
    v5 <- mkApp mkVersion vars5

    -- TODO [Ara] Should never suggest prerelease versions unless explicitly
    -- provided by the user.

    -- Create them with mkFreshVar and use them `varNames`
    foo <- mkFreshVar "foo" version
    bar <- mkFreshVar "bar" version
    baz <- mkFreshVar "baz" version

    Z3.Monad.assert =<< mkAnd =<< T.sequence
        [ mkVersionLE foo v1
        , mkVersionEQ foo v2
        , mkVersionGT bar v3
        , mkVersionEQ baz v4
        , mkVersionGE baz v5 ]

    (result, model) <- solverCheckAndGetModel

    case result of
        Sat -> pure model
        Unsat -> pure Nothing
        Undef -> pure Nothing

-- TODO [Ara] hoist this to MonadIO
-- TODO [Ara] should return ConstraintMap
solveConstraints :: ConstraintMap -> IO (Maybe Integer)
solveConstraints constraints = evalZ3 (constraintScript constraints) >>=
    \mbSol -> case mbSol of
        Nothing  -> pure Nothing
        Just sol -> pure $ Just 1

