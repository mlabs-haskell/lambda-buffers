{-# LANGUAGE  OverloadedLabels #-}

module LambdaBuffers.Common.TypeClass.Compat  where

import LambdaBuffers.Common.TypeClass.Rules
    ( Class(Class, name), Constraint(..), Instance, Rule((:<=)) )
import LambdaBuffers.Common.TypeClass.Pat
    ( toProd,
      toRec,
      toSum,
      Pat(AppP, (:*), Nil, (:=), DecP, Opaque, VarP, RefP, Name) )
import qualified LambdaBuffers.Common.ProtoCompat as P
import LambdaBuffers.Common.ProtoCompat.NameLike (coerceName)

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.State ( foldM, modify', evalState, State )
import qualified Data.Map as M
import Control.Lens ( (^.), view, set, over )
import Data.Generics.Labels (Field')
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.List (foldl')
import qualified Data.Set as S
import Control.Monad.Trans.Except ( ExceptT )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Data.Functor ((<&>))

type ForeignRefs = M.Map P.LBName P.ModuleName

type NameSpaced = State ForeignRefs

si :: P.SourceInfo
si = P.SourceInfo "" (P.SourcePosition 0 0) (P.SourcePosition 0 0)
-- need to initialize the sourceInfo field in both the keys and the values
sanitizeRefs :: ForeignRefs -> ForeignRefs
sanitizeRefs = M.fromList . map (bimap f goR) . M.toList
  where
    f :: forall s. Field' "sourceInfo" s P.SourceInfo => s -> s
    f = set #sourceInfo si

    goR :: P.ModuleName -> P.ModuleName
    goR = over #parts (map f) . f
{-
    TyDefs
-}

defToPat :: P.TyDef -> NameSpaced Pat
defToPat (P.TyDef tName (P.TyAbs tArgs tBody _) _) = DecP (Name $ tName ^. #name) vars <$> case tBody of
  P.SumI constrs -> toSum . NE.toList <$>  traverse  goConstr (constrs ^. #constructors)
  P.OpaqueI _ -> pure Opaque
  where
    collectFreeTyVars :: [P.TyArg] -> Pat
    collectFreeTyVars  = foldr (\x acc -> Name (x ^. (#argName . #name)) :* acc) Nil

    vars = collectFreeTyVars tArgs

    goConstr :: P.Constructor -> NameSpaced Pat
    goConstr (P.Constructor n p) = goProduct p >>= \prod -> pure $ Name (n ^. #name) := prod

    goProduct :: P.Product -> NameSpaced Pat
    goProduct = \case
      P.RecordI (P.Record rMap _) -> toRec  . NE.toList <$> traverse goField rMap
      P.TupleI (P.Tuple pList _)  -> toProd <$> traverse  tyToPat pList

    goField :: P.Field -> NameSpaced Pat
    goField (P.Field n v) = tyToPat v >>= \val -> pure $ Name (n ^. #name) := val

tyToPat :: P.Ty -> NameSpaced Pat
tyToPat = \case
  P.TyVarI t   -> pure $ VarP (t ^. #varName . #name)
  P.TyAppI tapp -> do
    fun <- tyToPat $ tapp ^. #tyFunc
    ps  <- traverse tyToPat $ tapp ^. #tyArgs
    pure $ appToPat fun ps
  P.TyRefI ref -> case ref of
    P.LocalI (P.LocalRef tn _) -> pure . RefP . Name $ tn ^. #name
    P.ForeignI (P.ForeignRef tn mn _) -> do
      modify' (M.insert (coerceName tn) mn)
      pure . RefP . Name $ (tn ^. #name)

appToPat :: Pat -> NonEmpty Pat -> Pat
appToPat fun (p :| ps) = case NE.nonEmpty ps of
      Nothing   -> AppP fun p
      Just rest -> AppP fun p `appToPat` rest

defToPat' :: P.TyDef -> Pat
defToPat' td = evalState (defToPat td) M.empty

{-
    Classes

    NOTE: We discard the arguments to the class and the arguments to the superclasses
          In the absence of MPTCs this should be an acceptable form of eta reduction

    NOTE: Actually previous note is probably wrong, this only allows for "direct superclasses"
          like (C a, D a) => E a. Or maybe it's not wrong? Without MPTCs, the only thing that
          wouldn't be direct is something like (C a, D a, E f) => G (f a b), but ATM we don't
          support type variables that represent kinds other than Star, so this should be
          good enough *for now*

    NOTE: MUST BE CHECKED FOR SUPERCLASS CYCLES OR WILL LOOP FOREVER
-}

-- could use a tuple but this makes the signatures more readable
data ClassInfo = ClassInfo {ciName :: Text, ciSupers :: [P.TyRef]}
  deriving stock (Show, Eq, Ord, Generic)

type Classes = S.Set Class

addRef :: forall n m
        . (P.NameLike n, MonadState ForeignRefs m)
       => n
       -> P.ModuleName
       -> m ()
addRef n mn = modify' (M.insert (coerceName n) mn)

-- utility
foldMWithKey :: Monad m => (b -> k -> a -> m b) -> b -> M.Map k a -> m b
foldMWithKey f b m = foldM (uncurry . f) b (M.toList m)

getClasses  :: [P.ClassDef] -> NameSpaced Classes
getClasses = resolveSupers . mkClassGraph . fmap defToClassInfo
  where
    defToClassInfo :: P.ClassDef -> ClassInfo
    defToClassInfo cd
      = ClassInfo (cd ^. #className . #name)
        $ map (\x -> x ^. #classRef) (cd ^. #supers)

    mkClassGraph :: [ClassInfo] -> M.Map Text [P.TyRef]
    mkClassGraph = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

    resolveSupers :: M.Map Text [P.TyRef] -> NameSpaced Classes
    resolveSupers cg = foldMWithKey  go S.empty cg
      where
        go :: Classes -> Text -> [P.TyRef] -> NameSpaced Classes
        go acc cname sups = cls <&> flip S.insert acc
          where
            cls :: NameSpaced Class
            cls = traverse collectSups sups <&> Class cname

            collectSups :: P.TyRef -> NameSpaced Class
            collectSups cn =  case cn of
              P.LocalI (P.LocalRef (view #name -> nm) _) ->  case M.lookup nm cg of
               Nothing -> pure $ Class nm []
               Just xs -> traverse collectSups xs <&> Class nm
              P.ForeignI (P.ForeignRef nm  mnm _) -> do
                modify' (M.insert (coerceName nm) mnm)
                case M.lookup (nm ^. #name) cg of
                 Nothing -> pure $ Class (nm ^. #name) []
                 Just xs -> do
                   suprs <- traverse collectSups xs
                   pure $ Class (nm ^. #name) suprs
{-
    Instances

    NOTE (mainly to self): Don't discard the arguments here! Need them!
-}

type Instances = S.Set Instance

data InstanceError  = UnknownClass P.TyRef P.SourceInfo

type InstanceM = ExceptT InstanceError NameSpaced

toClassTable :: Classes -> M.Map Text Class
toClassTable = foldl' (\acc c -> M.insert (name c) c acc) M.empty

getInstances :: M.Map Text Class -> [P.InstanceClause] -> InstanceM Instances
getInstances ctable  = foldM go S.empty
  where
    go :: S.Set Instance -> P.InstanceClause -> InstanceM Instances
    go acc (P.InstanceClause cn h csts si') = case cn of
      P.LocalI (P.LocalRef nm  _) -> case M.lookup (nm ^. #name) ctable of
        Nothing  -> throwError $ UnknownClass cn si'
        Just cls -> do
          p <- lift $ tyToPat h
          cs <- traverse goConstraint csts
          pure . flip  S.insert acc $ C cls p :<= cs
      P.ForeignI (P.ForeignRef nm mn _) -> case M.lookup (nm ^. #name) ctable of
        Nothing  -> throwError $  UnknownClass cn si
        Just cls -> do
          modify' $ M.insert (coerceName nm) mn
          p <- lift $  tyToPat h
          cs <- traverse goConstraint csts
          pure . flip  S.insert acc $ C cls p :<= cs

    goConstraint :: P.Constraint -> InstanceM Constraint
    goConstraint (P.Constraint cn arg si') = case cn of
      P.LocalI (P.LocalRef (view #name -> nm) _) -> case M.lookup nm ctable of
        Nothing   -> throwError $ UnknownClass cn si'
        Just cls  -> lift (tyToPat arg) <&>  C cls
      P.ForeignI (P.ForeignRef nm mn _) -> case M.lookup (nm ^. #name) ctable of
        Nothing -> throwError $ UnknownClass cn si
        Just cls -> do
          modify' $ M.insert (coerceName nm)  mn
          lift (tyToPat arg) <&> C cls
