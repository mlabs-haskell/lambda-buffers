{-# LANGUAGE  OverloadedLabels #-}

module LambdaBuffers.CodeGen.Common.Compat  where

import LambdaBuffers.CodeGen.Resolve.Rules
import LambdaBuffers.CodeGen.Common.Types
import qualified LambdaBuffers.Common.ProtoCompat as Ty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Control.Monad.State
import qualified Data.Map as M
import Control.Lens ( (^.) )
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.List (foldl')
import qualified Data.Set as S

type ForeignRefs = M.Map Ty.TyName Ty.ModuleName

type NameSpaced = State ForeignRefs

{-
    TyDefs
-}

defToPat :: Ty.TyDef -> NameSpaced Pat
defToPat (Ty.TyDef tName (Ty.TyAbs tArgs tBody _) _) = DecP (Name $ tName ^. #name) vars <$> case tBody of
  Ty.SumI constrs -> toSum . NE.toList <$>  traverse  goConstr (constrs ^. #constructors)
  Ty.OpaqueI _ -> pure Opaque
  where
    collectFreeTyVars :: [Ty.TyArg] -> Pat
    collectFreeTyVars  = foldr (\x acc -> Name (x ^. (#argName . #name)) :* acc) Nil

    vars = collectFreeTyVars tArgs

    goConstr :: Ty.Constructor -> NameSpaced Pat
    goConstr (Ty.Constructor n p) = goProduct p >>= \prod -> pure $ Name (n ^. #name) := prod

    goProduct :: Ty.Product -> NameSpaced Pat
    goProduct = \case
      Ty.RecordI (Ty.Record rMap _) -> toRec  . NE.toList <$> traverse goField  rMap
      Ty.TupleI (Ty.Tuple pList _)  -> toProd <$> traverse  tyToPat pList

    goField :: Ty.Field -> NameSpaced Pat
    goField (Ty.Field n v) = tyToPat v >>= \val -> pure $ Name (n ^. #name) := val

tyToPat :: Ty.Ty -> NameSpaced Pat
tyToPat = \case
  Ty.TyVarI t   -> pure $ VarP (t ^. #varName . #name)
  Ty.TyAppI tapp -> do
    fun <- tyToPat $ tapp ^. #tyFunc
    ps  <- traverse tyToPat $ tapp ^. #tyArgs
    pure $ appToPat fun ps
  Ty.TyRefI ref -> case ref of
    Ty.LocalI (Ty.LocalRef tn _) -> pure . RefP . Name $ tn ^. #name
    Ty.ForeignI (Ty.ForeignRef tn mn _) -> do
      modify' (M.insert tn mn)
      pure . RefP . Name $ (tn ^. #name)

appToPat :: Pat -> NonEmpty Pat -> Pat
appToPat fun (p :| ps) = case NE.nonEmpty ps of
      Nothing   -> AppP fun p
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

getClasses  :: [Ty.ClassDef] -> S.Set (Class l)
getClasses = resolveSupers . mkClassGraph . fmap defToClassInfo
  where
    defToClassInfo :: Ty.ClassDef -> ClassInfo
    defToClassInfo cd
      = ClassInfo (cd ^. #className . #name)
        $ map (\x -> x ^. #className . #name) (cd ^. #supers)

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
getInstances ctable  = foldM go S.empty
  where
    go :: S.Set (Instance l) -> Ty.InstanceClause -> Either (InstanceError l) (Instances l)
    go acc (Ty.InstanceClause cn h csts si) = case M.lookup (cn ^. #name) ctable of
      Nothing  -> Left $ UnknownClass cn si
      Just cls -> do
        let p = tyToPat' h
        cs <- traverse goConstraint csts
        pure . flip  S.insert acc $ C cls p :<= cs

    goConstraint :: Ty.Constraint -> Either (InstanceError l) (Constraint l)
    goConstraint (Ty.Constraint cn arg si) = case M.lookup (cn ^. #name) ctable of
      Nothing   -> Left $ UnknownClass cn si
      Just cls  -> Right $ C cls (tyToPat' arg)
