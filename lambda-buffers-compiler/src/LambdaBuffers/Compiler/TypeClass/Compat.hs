{-# LANGUAGE OverloadedLabels #-}

module LambdaBuffers.Compiler.TypeClass.Compat where

import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.ProtoCompat.NameLike (coerceName)
import LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (AppP, DecP, Name, Nil, Opaque, RefP, VarP, (:*), (:=)),
  toProd,
  toRec,
  toSum,
 )
import LambdaBuffers.Compiler.TypeClass.Rules

import Control.Lens ((^.), (^?))
import Control.Lens.Combinators
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (State, evalState, foldM, runState)
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Generics.Labels (Field')
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter

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
defToPat (P.TyDef tName (P.TyAbs tArgs tBody _) _) =
  DecP (Name $ tName ^. #name) vars <$> case tBody of
    P.SumI constrs -> toSum . NE.toList <$> traverse goConstr (constrs ^. #constructors)
    P.OpaqueI _ -> pure Opaque
  where
    collectFreeTyVars :: [P.TyArg] -> Pat
    collectFreeTyVars = foldr (\x acc -> Name (x ^. (#argName . #name)) :* acc) Nil

    vars = collectFreeTyVars tArgs

    goConstr :: P.Constructor -> NameSpaced Pat
    goConstr (P.Constructor n p) = goProduct p >>= \prod -> pure $ Name (n ^. #name) := prod

    goProduct :: P.Product -> NameSpaced Pat
    goProduct = \case
      P.RecordI (P.Record rMap _) -> toRec . NE.toList <$> traverse goField rMap
      P.TupleI (P.Tuple pList _) -> toProd <$> traverse tyToPat pList

    goField :: P.Field -> NameSpaced Pat
    goField (P.Field n v) = tyToPat v >>= \val -> pure $ Name (n ^. #name) := val

tyToPat :: P.Ty -> NameSpaced Pat
tyToPat = \case
  P.TyVarI t -> pure $ VarP (t ^. #varName . #name)
  P.TyAppI tapp -> do
    fun <- tyToPat $ tapp ^. #tyFunc
    ps <- traverse tyToPat $ tapp ^. #tyArgs
    pure $ appToPat fun ps
  P.TyRefI ref -> case ref of
    P.LocalI (P.LocalRef tn _) -> pure . RefP . Name $ tn ^. #name
    P.ForeignI (P.ForeignRef tn mn _) -> do
      modify' (M.insert (coerceName tn) mn)
      pure . RefP . Name $ (tn ^. #name)

appToPat :: Pat -> NonEmpty Pat -> Pat
appToPat fun (p :| ps) = case NE.nonEmpty ps of
  Nothing -> AppP fun p
  Just rest -> AppP fun p `appToPat` rest

defToPat' :: P.TyDef -> Pat
defToPat' td = evalState (defToPat td) M.empty

prettyClassRef :: ClassRef -> Doc ()
prettyClassRef (CRef nm mn) = prettyModuleName mn <> "." <> pretty (nm ^. #name)

prettyModuleName :: P.ModuleName -> Doc ()
prettyModuleName (P.ModuleName parts _) = hcat . punctuate "." $ map (\x -> pretty $ x ^. #name) parts

prettyClass :: Class -> Doc ()
prettyClass (Class nm []) = prettyClassRef nm
prettyClass (Class nm sups) = prettyClassRef nm <+> "<=" <+> hcat (map prettyClass sups)

prettyConstraint :: Constraint -> Doc ()
prettyConstraint (C cls p) = "C" <+> parens (prettyClass cls) <+> viaShow p

prettyInstance :: Instance -> Doc ()
prettyInstance (c :<= cs) = prettyConstraint c <+> "<=" <+> list (prettyConstraint <$> cs)
