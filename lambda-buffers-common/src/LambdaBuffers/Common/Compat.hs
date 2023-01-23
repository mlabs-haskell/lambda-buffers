{-# LANGUAGE OverloadedLabels #-}

{- This is my take on what our proto *should* look like.

Everything that operates on this can easily be adapted to operate on something similar, but
it will make many functions partial or introduce the need for additional error handling if
we end up abandoning the use of NonEmpty/etc.

-}
module LambdaBuffers.Common.Compat where

import Control.Lens ((^.))
import Control.Monad.State
import Data.Generics.Labels ()
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import GHC.Generics (Generic)
import LambdaBuffers.Common.Types
import LambdaBuffers.ProtoCompat.ProtoCompat qualified as Ty
import LambdaBuffers.Resolve.Rules

type ForeignRefs = M.Map Ty.TyName Ty.ModuleName

type NameSpaced = State ForeignRefs

{-
    TyDefs
-}

defToPat :: Ty.TyDef -> NameSpaced Pat
defToPat (Ty.TyDef tName tArgs tBody _) =
  DecP (Name $ tName ^. #name) vars <$> case tBody ^. #tyBody of
    Ty.SumI constrs -> toSum . NE.toList <$> traverse goConstr (constrs ^. #constructors)
    Ty.OpaqueI _ -> pure Opaque
  where
    collectFreeTyVars :: [Ty.TyArg] -> Pat
    collectFreeTyVars = foldr (\x acc -> Name (x ^. (#argName . #name)) :* acc) Nil

    vars = collectFreeTyVars tArgs

    goConstr :: Ty.Constructor -> NameSpaced Pat
    goConstr (Ty.Constructor n p) = goProduct p >>= \prod -> pure $ Name (n ^. #name) := prod

    goProduct :: Ty.Product -> NameSpaced Pat
    goProduct p = case p ^. #product of
      Ty.EmptyI -> pure Nil
      Ty.RecordI rMap -> toRec . NE.toList <$> traverse goField rMap
      Ty.TupleI pList -> toProd <$> traverse tyToPat pList

    goField :: Ty.Field -> NameSpaced Pat
    goField (Ty.Field n v) = tyToPat v >>= \val -> pure $ Name (n ^. #name) := val

tyToPat :: Ty.Ty -> NameSpaced Pat
tyToPat tty = case tty ^. #ty of
  Ty.TyVarI t -> pure $ VarP (t ^. #varName . #name)
  Ty.TyAppI tapp -> do
    fun <- tyToPat $ tapp ^. #tyFunc
    ps <- traverse tyToPat $ tapp ^. #tyArgs
    pure $ appToPat fun ps
  Ty.TyRefI ref -> case ref ^. #tyRef of
    Ty.LocalI t -> pure . RefP . Name $ t ^. #name
    Ty.ForeignI (Ty.ForeignRef tn mn) -> do
      modify' (M.insert tn mn)
      pure . RefP . Name $ (tn ^. #name)

appToPat :: Pat -> NonEmpty Pat -> Pat
appToPat fun (p :| ps) = case NE.nonEmpty ps of
  Nothing -> AppP fun p
  Just rest -> AppP fun p `appToPat` rest

-- ignore namespacing, for converting types in an instance head or context to patterns.
-- TODO: Make sure ignoring namespacing is actually OK there...
tyToPat' :: Ty.Ty -> Pat
tyToPat' = flip evalState M.empty . tyToPat

{-
    Classes

    NOTE: We discard the arguments to the class and the arguments to the superclasses
          In the absence of MPTCs this should be an acceptable form of eta reduction

    NOTE: MUST BE CHECKED FOR SUPERCLASS CYCLES OR WILL LOOP FOREVER
-}

-- could use a tuple but this makes the signatures more readable
data ClassInfo = ClassInfo {ciName :: Text, ciSupers :: [Text]}
  deriving stock (Show, Eq, Ord, Generic)

getClasses :: [Ty.ClassDef] -> S.Set (Class l)
getClasses = resolveSupers . mkClassGraph . fmap defToClassInfo
  where
    defToClassInfo :: Ty.ClassDef -> ClassInfo
    defToClassInfo cd =
      ClassInfo (cd ^. #className . #name) $
        map (\x -> x ^. #className . #name) (cd ^. #supers)

    mkClassGraph :: [ClassInfo] -> M.Map Text [Text]
    mkClassGraph = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

    resolveSupers :: M.Map Text [Text] -> S.Set (Class l)
    resolveSupers cg = M.foldlWithKey go S.empty cg
      where
        go :: S.Set (Class l) -> Text -> [Text] -> S.Set (Class l)
        go acc cname sups = S.insert cls acc
          where
            cls :: Class l
            cls = Class cname (collectSups <$> sups)

            collectSups :: Text -> Class l
            collectSups cn = Class cn $ case M.lookup cn cg of
              Nothing -> []
              Just xs -> collectSups <$> xs

{-
    Instances

    NOTE (mainly to self): Don't discard the arguments here! Need them!
-}

type Instances l = S.Set (Instance l)

data InstanceError l = UnknownClass Ty.ClassName Ty.SourceInfo

toClassTable :: S.Set (Class l) -> M.Map Text (Class l)
toClassTable = foldl' (\acc c -> M.insert (name c) c acc) M.empty

getInstances :: forall l. M.Map Text (Class l) -> [Ty.InstanceClause] -> Either (InstanceError l) (Instances l)
getInstances ctable = foldM go S.empty
  where
    go :: S.Set (Instance l) -> Ty.InstanceClause -> Either (InstanceError l) (Instances l)
    go acc (Ty.InstanceClause cn h csts si) = case M.lookup (cn ^. #name) ctable of
      Nothing -> Left $ UnknownClass cn si
      Just cls -> do
        let p = tyToPat' h
        cs <- traverse goConstraint csts
        pure . flip S.insert acc $ C cls p :<= cs

    goConstraint :: Ty.Constraint -> Either (InstanceError l) (Constraint l)
    goConstraint (Ty.Constraint cn arg si) = case M.lookup (cn ^. #name) ctable of
      Nothing -> Left $ UnknownClass cn si
      Just cls -> Right $ C cls (tyToPat' arg)
