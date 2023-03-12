module LambdaBuffers.Compiler.TypeClassCheck.SuperclassCycleCheck (runCheck) where

import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Data.Foldable (Foldable (toList), for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.Errors (superClassCycleDetectedError, unboundTyClassRefError)
import Proto.Compiler qualified as P

data CheckRead = MkCheckRead
  { currentModuleName :: PC.ModuleName
  , currentClassName :: PC.ClassName
  , classDefs :: Map (PC.InfoLess PC.ModuleName, PC.InfoLess PC.ClassName) PC.ClassDef
  , trace :: Set (PC.InfoLess PC.ClassName)
  , reportingTrace :: [PC.TyClassRef]
  }

type MonadCheck m = (MonadReader CheckRead m, MonadError P.TyClassCheckError m)

runCheck :: PC.CompilerInput -> Either [P.TyClassCheckError] ()
runCheck ci =
  let -- Index the class definitions by a qualified name (ModuleName, ClassName).
      classDefs = PC.indexClassDefs ci
      -- Process each type class definitions under the appropriate reader context and collect errors.
      allErrors =
        concat $
          runCheckOnClassDef classDefs
            <$> [ (m ^. #moduleName, cd)
                | m <- toList $ ci ^. #modules
                , cd <- toList $ m ^. #classDefs
                ]
   in if null allErrors then Right () else Left allErrors

runCheckOnClassDef :: Map PC.QClassName PC.ClassDef -> (PC.ModuleName, PC.ClassDef) -> [P.TyClassCheckError]
runCheckOnClassDef classDefs (mn, cd) =
  let errM = runReaderT (checkClassDef cd) (MkCheckRead mn (cd ^. #className) classDefs Set.empty [])
   in case runExcept errM of
        Left err -> [err]
        Right _ -> []

checkClassDef :: MonadCheck m => PC.ClassDef -> m ()
checkClassDef (PC.ClassDef cn _ sups _ _) = do
  checkCycle cn
  for_
    sups
    ( \c -> do
        cds <- asks classDefs
        mn <- asks currentModuleName
        let qcn = PC.qualifyClassRef mn (c ^. #classRef)
        case Map.lookup qcn cds of
          Nothing -> throwUnboundTyClassRef c
          Just cd -> do
            local
              ( \r ->
                  r
                    { trace = Set.union (trace r) (Set.singleton . PC.mkInfoLess $ cn)
                    , reportingTrace = (c ^. #classRef) : reportingTrace r
                    }
              )
              (checkClassDef cd)
    )

checkCycle :: MonadCheck m => PC.ClassName -> m ()
checkCycle cn = do
  trace <- asks trace
  when (Set.member (PC.mkInfoLess cn) trace) (void throwCycleDetected)

throwCycleDetected :: MonadCheck m => m a
throwCycleDetected = do
  mn <- asks currentModuleName
  currcn <- asks currentClassName
  rtrace <- asks reportingTrace
  throwError $ superClassCycleDetectedError mn currcn rtrace

throwUnboundTyClassRef :: MonadCheck m => PC.ClassConstraint -> m a
throwUnboundTyClassRef cc = do
  mn <- asks currentModuleName
  throwError $ unboundTyClassRefError mn (cc ^. #classRef)
