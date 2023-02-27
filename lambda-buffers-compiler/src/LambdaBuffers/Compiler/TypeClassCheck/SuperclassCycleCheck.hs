module LambdaBuffers.Compiler.TypeClassCheck.SuperclassCycleCheck (runCheck) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Data.Foldable (Foldable (foldl', toList), for_)
import Data.Generics.Product (HasField)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields (unboundClassRefErr)
import Proto.Compiler_Fields qualified as P

data CheckRead = MkCheckRead
  { currentModuleName :: PC.ModuleName
  , currentClassName :: PC.ClassName
  , classDefs :: Map (Stripped PC.ModuleName, Stripped PC.ClassName) PC.ClassDef
  , trace :: Set (Stripped PC.ClassName)
  , reportingTrace :: [PC.TyClassRef]
  }

type MonadCheck m = (MonadReader CheckRead m, MonadError P.TyClassCheckError m)

runCheck :: PC.CompilerInput -> Either [P.TyClassCheckError] ()
runCheck ci =
  let -- Index the class definitions by a qualified name (ModuleName, ClassName)
      classDefs = Map.fromList [((stripSi mn, stripSi cn), cd) | (mn, m) <- Map.toList $ ci ^. #modules, (cn, cd) <- Map.toList $ m ^. #classDefs]
      -- Process each type class definitions under the appropriate reader context and collect errors
      allErrors =
        foldl'
          ( \errs (mn, cd) ->
              let errM = runReaderT (checkClassDef cd) (MkCheckRead mn (cd ^. #className) classDefs Set.empty [])
               in case runExcept errM of
                    Left err -> err : errs
                    Right _ -> errs
          )
          []
          [(m ^. #moduleName, cd) | m <- toList $ ci ^. #modules, cd <- toList $ m ^. #classDefs]
   in if null allErrors then Right () else Left allErrors

checkClassDef :: MonadCheck m => PC.ClassDef -> m ()
checkClassDef (PC.ClassDef cn _ sups _ _) = do
  checkCycle cn
  for_
    sups
    ( \c -> do
        cds <- asks classDefs
        qcn <- classRefToQClassName $ c ^. #classRef
        case Map.lookup qcn cds of
          Nothing -> throwUnboundTyClassRef c
          Just cd -> do
            local
              ( \r ->
                  r
                    { trace = Set.union (trace r) (Set.singleton . stripSi $ cn)
                    , reportingTrace = (c ^. #classRef) : reportingTrace r
                    }
              )
              (checkClassDef cd)
    )

checkCycle :: MonadCheck m => PC.ClassName -> m ()
checkCycle cn = do
  trace <- asks trace
  when (Set.member (stripSi cn) trace) $ do
    mn <- asks currentModuleName
    currcn <- asks currentClassName
    rtrace <- asks reportingTrace
    throwError $
      defMessage
        & P.superclassCycleErr . P.moduleName .~ PC.toProto mn
        & P.superclassCycleErr . P.className .~ PC.toProto currcn
        & P.superclassCycleErr . P.cycledClassRefs .~ (PC.toProto <$> rtrace)

throwUnboundTyClassRef :: MonadCheck m => PC.Constraint -> m ()
throwUnboundTyClassRef c = do
  mn <- asks currentModuleName
  throwError $
    defMessage
      & unboundClassRefErr . P.moduleName .~ PC.toProto mn
      & unboundClassRefErr . P.classRef .~ PC.toProto (c ^. #classRef)

classRefToQClassName :: MonadCheck m => PC.TyClassRef -> m (Stripped PC.ModuleName, Stripped PC.ClassName)
classRefToQClassName (PC.LocalCI lcr) = do
  mn <- asks currentModuleName
  return (stripSi mn, stripSi $ lcr ^. #className)
classRefToQClassName (PC.ForeignCI fcr) = return (stripSi $ fcr ^. #moduleName, stripSi $ fcr ^. #className)

-- | TODO: Use cstml's machinery, this is just temporary.
newtype Stripped a = MkStripped a deriving stock (Eq, Ord, Show)

stripSi :: HasField "sourceInfo" s a1 a2 PC.SourceInfo => s -> Stripped a1
stripSi x = MkStripped $ PC.stripSourceInfo x
