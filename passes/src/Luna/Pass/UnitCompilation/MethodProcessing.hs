{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.UnitCompilation.MethodProcessing where

import Luna.Prelude hiding (String, s, new, Constructor, Destructor, cons)
import           OCI.Pass         (SubPass, Pass)
import qualified OCI.Pass         as Pass
import qualified OCI.Pass.Manager as Pass
import qualified Luna.IR.Expr     as Term
import OCI.IR.Combinators
import qualified OCI.IR.Class as Event
import Luna.Builtin.Data.Class    as Class
import Luna.Builtin.Data.Module
import Luna.Builtin.Data.Function
import Luna.Builtin.Data.LunaValue (LunaValue)
import Luna.IR hiding (Function)
import qualified Control.Monad.State.Dependent as Dep
import Control.Monad.Trans.State
import Luna.Pass.Data.UniqueNameGen
import qualified Luna.Pass.UnitCompilation.RecordProcessing as RecordProcessing

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Lazy.Merge as Map
import qualified Data.Set as Set
import Luna.Pass.Evaluation.Interpreter
import Luna.Pass.Typechecking.Typecheck
import Luna.Pass.Inference.Data.Unifications    (Unifications)
import Luna.Pass.Inference.Data.SimplifierQueue (SimplifierQueue)
import Luna.Pass.Inference.Data.MergeQueue      (MergeQueue)
import Luna.Pass.Resolution.Data.UnresolvedAccs (UnresolvedAccs, getAccs)
import Luna.Pass.Resolution.Data.CurrentTarget
import Luna.Pass.UnitCompilation.DefProcessing
import Data.TypeDesc

import Data.Text32 (Text32)

import Luna.Test.IR.Runner
import Data.Future (delay, Future)

data MethodProcessing
type instance Abstract   MethodProcessing = MethodProcessing
type instance Pass.Inputs     Net   MethodProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer MethodProcessing = '[AnyExpr // Model, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // Model]
type instance Pass.Inputs     Attr  MethodProcessing = '[UniqueNameGen]
type instance Pass.Inputs     Event MethodProcessing = '[]

type instance Pass.Outputs    Net   MethodProcessing = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer MethodProcessing = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Succs]
type instance Pass.Outputs    Attr  MethodProcessing = '[UniqueNameGen]
type instance Pass.Outputs    Event MethodProcessing = '[Event.Import // AnyExpr, Event.Import // AnyExprLink, New // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink, New // AnyExprLink, OnDeepDelete // AnyExpr]

type instance Pass.Preserves        MethodProcessing = '[]

processMethods :: (MonadPassManager m, MonadIO m, Dep.MonadState Cache m) => Name -> Imports -> Name -> [Name] -> [Name] -> [(Name, Maybe Text32, Rooted SomeExpr)] -> m (Map Name (WithDocumentation (Either [CompileError] Function)))
processMethods modName imps className classParamNames consNames methodIRs = mdo
    result     <- liftIO $ forM methodIRs $ \(n, doc, body) ->
        fmap (WithDocumentation doc) $ delay $
            mkMethod modName allImports className classParamNames n body
    let duplicates = checkUniqueness $ map (view _1) methodIRs
        meths      = zip (view _1 <$> methodIRs) result
        allImports = imps & importedClasses . ix className . documentedItem . Class.methods %~ Map.union (Map.fromList meths)
        resultMap  = Map.fromList meths
    case duplicates of
        Nothing               -> return resultMap
        Just duplicateMethods -> do
            let errorText n   = "Method " <> convert n <> " of "
                             <> convert className <> " class, "
                             <> "defined in module " <> convert modName
                             <> " has multiple definitions."
                methodError n = Left $ [CompileError (errorText n) [] []]
                errorsMap = Map.fromList
                          $ map (\a -> (a, methodError a)) duplicateMethods
            return $ Map.merge
                        Map.preserveMissing
                        Map.dropMissing
                        (Map.zipWithMatched $
                            \key (WithDocumentation doc _) error ->
                                WithDocumentation doc error)
                        resultMap
                        errorsMap

checkUniqueness :: [Name] -> Maybe [Name]
checkUniqueness methodNames =
    let multiset   = Map.fromListWith (+) $ map (,1) methodNames
        duplicates = Map.filter (> 1) multiset
    in if Map.null duplicates then Nothing else Just (Map.keys duplicates)

mkMethod ::  Name -> Imports -> Name -> [Name] -> Name -> Rooted SomeExpr -> IO (Either [CompileError] Function)
mkMethod modName imports className classParamNames methodName methodIR@(Rooted _ methodRoot) = fmap (\(Right x) -> x) $ runPM False $ do
    runRegs
    initNameGen
    methodRoot <- Pass.eval' @MethodProcessing $ do
        root        <- ($ methodRoot) <$> importRooted methodIR
        self        <- var "self"
        classParams <- mapM var classParamNames
        selfType    <- cons className classParams
        monad       <- var =<< genName
        selfTypeInM <- monadic selfType monad

        oldTp <- getLayer @Type self >>= source
        reconnectLayer @Type selfTypeInM self
        deleteSubtree oldTp

        lambda <- lam self root
        return $ unsafeGeneralize lambda
    mkDef modName imports (TgtMethod modName className methodName) methodRoot
