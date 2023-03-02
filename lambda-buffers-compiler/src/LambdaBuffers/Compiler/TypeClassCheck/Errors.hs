module LambdaBuffers.Compiler.TypeClassCheck.Errors (
  type Instance,
  TypeClassError (UnknownClass, UnknownModule, MissingModuleInstances, MissingModuleScope, ClassNotFoundInModule, LocalTyRefNotFound, SuperclassCycleDetected, FailedToSolveConstraints, MalformedTyDef, BadInstance),
  BasicConditionViolation (TyConInContext, OverlapDetected, OnlyTyVarsInHead),
  tcErrToProto,
) where

import GHC.Generics (Generic)

import Data.Text qualified as T

import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Class (Class),
  Constraint (C),
  FQClassName (FQClassName),
  Rule ((:<=)),
 )

import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC (
  ClassName (ClassName),
  ConstrName (ConstrName),
  Constraint (Constraint),
  FieldName (FieldName),
  ForeignClassRef (ForeignClassRef),
  ForeignRef (ForeignRef),
  InstanceClause (InstanceClause),
  LBName (LBName),
  LocalClassRef (LocalClassRef),
  LocalRef (LocalRef),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  SourceInfo,
  Ty (TyAppI, TyRefI, TyVarI),
  TyApp (TyApp),
  TyClassRef (ForeignCI, LocalCI),
  TyName (TyName),
  TyRef (ForeignI, LocalI),
  TyVar (TyVar),
  VarName (..),
 )
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp (AppE, LitE, NilE, RefE), Literal (ModuleName, Name, TyVar), Pat (AppP, LitP, NilP, RefP), Tagged, getTag, unTag, unTyFunE, unTyFunP)

import LambdaBuffers.Compiler.TypeClassCheck.Pretty (pointies, (<///>))
import LambdaBuffers.Compiler.TypeClassCheck.Solve (Overlap (Overlap))
import Prettyprinter (
  Doc,
  Pretty (pretty),
  defaultLayoutOptions,
  hcat,
  indent,
  layoutPretty,
  line,
  nest,
  punctuate,
  vcat,
  (<+>),
 )

import Control.Lens.Lens ((&))
import Control.Lens.Operators ((.~))
import Data.ProtoLens (defMessage)
import LambdaBuffers.Compiler.ProtoCompat.FromProto (toProto)
import Prettyprinter.Render.Text (renderStrict)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  MonadTrans (lift),
  runExceptT,
 )
import Control.Monad.Reader (
  MonadReader (reader),
  Reader,
  runReader,
 )
import Data.Generics.Labels (Field')
import LambdaBuffers.Compiler.ProtoCompat.Types (defSourceInfo)

type Instance = Rule Pat

{- Some of these are perfunctory & used to keep functions total.
-}
data TypeClassError
  = UnknownClass FQClassName PC.SourceInfo -- this might need split into two, investigate further
  | UnknownModule PC.ModuleName -- internal, no sourceInfo (doesn't make sense)
  | MissingModuleInstances PC.ModuleName -- internal, no sourceInfo (doesn't make sense)
  | MissingModuleScope PC.ModuleName -- internal, no sourceinfo (doesn't make sense)
  | ClassNotFoundInModule Text [Text] -- internal-ish? this one's weird. there's not really one place in the source that triggers it. come back to it later
  | LocalTyRefNotFound T.Text PC.ModuleName PC.SourceInfo -- SI is the instance clause that triggered the tyref lookup
  | SuperclassCycleDetected [[FQClassName]] -- No sourceinfo, it's not very useful due to the nature of cycles
  | FailedToSolveConstraints PC.ModuleName [Tagged (Constraint Exp)] Instance PC.SourceInfo -- SI is the instance clause that triggered the subgoal that failed (subgoal itself may not exist anywhere in the source)
  | MalformedTyDef PC.ModuleName Exp PC.SourceInfo -- SI is tydef
  | BadInstance BasicConditionViolation PC.SourceInfo -- SI is the source of the rule that triggered the violation. This might be weird
  deriving stock (Show, Eq, Generic)

instance Pretty TypeClassError where
  pretty = \case
    UnknownClass cref i ->
      "Error at" <+> pretty i <+> nest 2 ("Unknown class: " <> pretty cref)
    UnknownModule mn ->
      "INTERNAL ERROR: Unknown Module" <+> pretty mn
    MissingModuleInstances mn ->
      "INTERNAL ERROR: Missing instance data for module" <+> pretty mn
    MissingModuleScope mn ->
      "INTERNAL ERROR: Could not determine TypeClass scope for module" <+> pretty mn
    ClassNotFoundInModule cn mn ->
      "Error: Expected to find class"
        <+> pretty cn
        <+> "in module"
        <+> pretty mn
        <+> "but it isn't there!"
    LocalTyRefNotFound txt mn _ ->
      "Error: Expected to find a type definition for a type named"
        <+> pretty txt
        <+> "in module"
        <+> pretty mn
        <+> "but it isn't there!"
    SuperclassCycleDetected crs ->
      "Error: Superclass cycles detected in compiler input:"
        <+> nest
          2
          ( vcat
              . map (hcat . punctuate " => " . map pretty)
              $ crs
          )
    FailedToSolveConstraints mn cs i _ ->
      "Error: Could not derive instance:"
        <+> pointies (pretty i)
          <> line
          <> line
          <> indent 2 "in module"
        <+> pretty mn
          <> line
          <> line
          <> indent 2 "because the following constraint(s) were not satisfied:"
          <> line
          <> line
          <> indent 2 (vcat $ map pretty cs)
    MalformedTyDef mn xp _ ->
      "Error: Encountered malformed type definition:"
        <> line
        <> indent 2 (pretty xp)
        <> line
        <> indent 2 ("in module" <+> pretty mn)
    BadInstance bcv _ -> pretty bcv

data BasicConditionViolation
  = TyConInContext Instance (Constraint Pat)
  | OnlyTyVarsInHead Instance (Constraint Pat)
  | OverlapDetected Overlap
  deriving stock (Show, Eq, Generic)

instance Pretty BasicConditionViolation where
  pretty = \case
    TyConInContext inst cst ->
      "Error: Invalid instance declaration!"
        <///> indent 2 (pretty inst)
        <///> "The instance constraint"
        <///> indent 2 (pretty cst)
        <///> "Contains a type constructor, but may only contain type variables"
    OnlyTyVarsInHead inst cst ->
      "Error: Invalid instance declaration!"
        <///> indent 2 (pretty inst)
        <///> "The instance constraint"
        <///> indent 2 (pretty cst)
        <///> "only contains type variables, but must contain at least one constructor"
    OverlapDetected (Overlap cst rules) ->
      "Error: Overlapping instances detected when trying to solve constraint"
        <+> pretty cst
          <> line
          <> indent 2 (vcat (map pretty rules))

mkMsg :: Pretty a => a -> Text
mkMsg = renderP . pretty

renderP :: Doc () -> Text
renderP = renderStrict . layoutPretty defaultLayoutOptions

-- One way conversion; don't think we'll ever need to convert back into Haskell types (also: we can't)
tcErrToProto :: TypeClassError -> Either P.InternalError P.TypeClassCheckError
tcErrToProto = \case
  UnknownClass fqn i ->
    Right $
      defMessage
        & P.unknownClass
          .~ ( defMessage
                & P.classRef .~ toProto (unSI i (siClassRef fqn))
             )
  LocalTyRefNotFound tn mn i ->
    Right $
      defMessage
        & P.refNotFound
          .~ ( defMessage
                & P.tyName .~ toProto (def $ siName @PC.TyName tn)
                & P.moduleName .~ toProto mn
                & P.sourceInfo .~ toProto i
             )
  scCycles@(SuperclassCycleDetected _) ->
    Right $
      defMessage
        & P.superclassCycle .~ (defMessage & P.msg .~ mkMsg scCycles)
  FailedToSolveConstraints mn cs inst i -> case traverse (\x -> convertConstraintE (getTag x) (unTag x)) cs of
    Left e -> Left $ handleErr e
    Right cs' -> case convertInstance i inst of
      Left e -> Left $ handleErr e
      Right inst' ->
        Right $
          defMessage
            & P.failedToSolveConstraints
              .~ ( defMessage
                    & P.moduleName .~ toProto mn
                    & P.constraints .~ map toProto cs'
                    & P.instance' .~ toProto inst'
                 )
  BadInstance (OverlapDetected (Overlap cst rules)) _ -> case convertConstraintE (getTag cst) (unTag cst) of
    Left e -> Left $ handleErr e
    Right cst' -> case traverse (\x -> convertInstance (getTag x) (unTag x)) rules of
      Left e -> Left $ handleErr e
      Right insts' ->
        Right $
          defMessage
            & P.overlappingInstances
              .~ ( defMessage
                    & P.constraint .~ toProto @P.Constraint cst'
                    & P.overlaps .~ map toProto insts'
                 )
  internal -> Left $ defMessage & P.msg .~ mkMsg internal
  where
    handleErr :: ConversionError -> P.InternalError
    handleErr = \case
      MalformedTypeApplication e ->
        defMessage
          & P.msg
            .~ renderP
              ( "INTERNAL ERROR: Malformed type application "
                  <+> either pretty pretty e
              )
      NotATy e ->
        defMessage
          & P.msg
            .~ renderP
              ( "INTERNAL ERROR: Not a type "
                  <+> either pretty pretty e
              )

-- TOOLS FOR DEALING W/ SOURCEINFO & CONVERSIONS

data ConversionError
  = MalformedTypeApplication (Either Pat Exp) -- this is actually impossible in the context where it's thrown but need it for totality
  | NotATy (Either Pat Exp) -- If we get a failure on a rule where the head doesn't correspond to any possible PC.Ty, e.g. structural rules

-- Yes, this is actually 1000x more convenient than doing it by hand
type SI = Reader PC.SourceInfo

unSI :: PC.SourceInfo -> SI t -> t
unSI = flip runReader

si :: (PC.SourceInfo -> t) -> SI t
si = reader

siModulename :: [Text] -> SI PC.ModuleName
siModulename xs = traverse siName xs >>= si . PC.ModuleName

siClassRef :: FQClassName -> SI PC.TyClassRef
siClassRef (FQClassName cn mn) =
  siName cn >>= \cn' -> case mn of
    [] -> si $ PC.LocalCI . PC.LocalClassRef cn'
    _ ->
      siModulename mn >>= \mn' ->
        si $ PC.ForeignCI . PC.ForeignClassRef cn' mn'

siConstraintE :: Constraint Exp -> ExceptT ConversionError SI PC.Constraint
siConstraintE (C (Class fqcn _) xp) = do
  cref <- lift $ siClassRef fqcn
  ty <- expToTy xp
  lift . si $ PC.Constraint cref ty

def :: SI t -> t
def = unSI defSourceInfo

convertConstraintE :: PC.SourceInfo -> Constraint Exp -> Either ConversionError PC.Constraint
convertConstraintE i = unSI i . runExceptT . siConstraintE

siConstraintP :: Constraint Pat -> ExceptT ConversionError SI PC.Constraint
siConstraintP (C (Class fqcn _) xp) = do
  cref <- lift $ siClassRef fqcn
  ty <- patToTy xp
  lift . si $ PC.Constraint cref ty

convertConstraintP :: PC.SourceInfo -> Constraint Pat -> Either ConversionError PC.Constraint
convertConstraintP i = unSI i . runExceptT . siConstraintP

convertConstraintP' :: Constraint Pat -> Either ConversionError PC.Constraint
convertConstraintP' = convertConstraintP defSourceInfo

convertInstance :: PC.SourceInfo -> Instance -> Either ConversionError PC.InstanceClause
convertInstance i (C (Class cr _) xp :<= rest) = do
  let cr' = def $ siClassRef cr
  hd <- def $ runExceptT (patToTy xp)
  cxt <- traverse convertConstraintP' rest
  pure $ PC.InstanceClause cr' hd cxt i

expToTy :: Exp -> ExceptT ConversionError SI PC.Ty
expToTy = \case
  LitE (TyVar t) -> do
    t' <- lift $ siName t
    tv <- lift . si $ PC.TyVar t'
    pure $ PC.TyVarI tv
  app@(AppE _ _) -> case unTyFunE app of
    Nothing -> throwError $ MalformedTypeApplication (Right app) -- unTyFunE succeeds for every AppE, this really is impossible here
    Just (f, args) -> do
      tfunc <- expToTy f
      targs <- traverse expToTy args
      tapp <- lift . si $ PC.TyApp tfunc targs
      pure $ PC.TyAppI tapp
  RefE NilE (LitE (Name t)) -> do
    t' <- lift $ siName t
    lift . si $ PC.TyRefI . PC.LocalI . PC.LocalRef t'
  RefE (LitE (ModuleName xs)) (LitE (Name t)) -> do
    mn <- lift $ siModulename xs
    t' <- lift $ siName t
    lift . si $ PC.TyRefI . PC.ForeignI . PC.ForeignRef t' mn
  other -> throwError $ NotATy (Right other)

patToTy :: Pat -> ExceptT ConversionError SI PC.Ty
patToTy = \case
  LitP (TyVar t) -> do
    t' <- lift $ siName t
    tv <- lift . si $ PC.TyVar t'
    pure $ PC.TyVarI tv
  app@(AppP _ _) -> case unTyFunP app of
    Nothing -> throwError $ MalformedTypeApplication (Left app) -- unTyFunE succeeds for every AppE, this really is impossible here
    Just (f, args) -> do
      tfunc <- patToTy f
      targs <- traverse patToTy args
      tapp <- lift . si $ PC.TyApp tfunc targs
      pure $ PC.TyAppI tapp
  RefP NilP (LitP (Name t)) -> do
    t' <- lift $ siName t
    lift . si $ PC.TyRefI . PC.LocalI . PC.LocalRef t'
  RefP (LitP (ModuleName xs)) (LitP (Name t)) -> do
    mn <- lift $ siModulename xs
    t' <- lift $ siName t
    lift . si $ PC.TyRefI . PC.ForeignI . PC.ForeignRef t' mn
  other -> throwError $ NotATy (Left other)

-- Helper type class for conversions. Ripped this out of one of my old PRs
class (Field' "name" s Text, Field' "sourceInfo" s PC.SourceInfo) => NameLike s where
  ctorName :: Text -> PC.SourceInfo -> s

instance NameLike PC.LBName where
  ctorName = PC.LBName

instance NameLike PC.TyName where
  ctorName = PC.TyName

instance NameLike PC.ConstrName where
  ctorName = PC.ConstrName

instance NameLike PC.ModuleNamePart where
  ctorName = PC.ModuleNamePart

instance NameLike PC.VarName where
  ctorName = PC.VarName

instance NameLike PC.FieldName where
  ctorName = PC.FieldName

instance NameLike PC.ClassName where
  ctorName = PC.ClassName

siName :: NameLike s => Text -> SI s
siName = reader . ctorName
