module LambdaBuffers.Compiler.TypeClassCheck.Errors (
  type Instance,
  TypeClassError (UnknownClass, UnknownModule, MissingModuleInstances, MissingModuleScope, ClassNotFoundInModule, LocalTyRefNotFound, SuperclassCycleDetected, FailedToSolveConstraints, MalformedTyDef, BadInstance),
  BasicConditionViolation (TyConInContext, OverlapDetected, OnlyTyVarsInHead),
  tcErrToProto,
) where

import GHC.Generics (Generic)

import Data.Text qualified as T

import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Constraint,
  FQClassName,
  Rule,
 )

import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC (
  ModuleName,
  SourceInfo,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp, Pat)

import LambdaBuffers.Compiler.TypeClassCheck.Pretty (pointies, (<///>))
import LambdaBuffers.Compiler.TypeClassCheck.Solve (Overlap (Overlap))
import Prettyprinter (
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
  | FailedToSolveConstraints PC.ModuleName [Constraint Exp] Instance PC.SourceInfo -- SI is the instance clause that triggered the subgoal that failed (subgoal itself may not exist anywhere in the source)
  | MalformedTyDef PC.ModuleName Exp PC.SourceInfo -- SI is the BODY of the exp
  | BadInstance BasicConditionViolation PC.SourceInfo -- SI is the source of the rule that triggered the violation. This might be weird
  deriving stock (Show, Eq, Generic)

instance Pretty TypeClassError where
  pretty = \case
    UnknownClass cref si ->
      "Error at" <+> pretty si <+> nest 2 ("Unknown class: " <> pretty cref)
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

mkMsg :: TypeClassError -> Text
mkMsg = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- One way conversion; don't think we'll ever need to convert back into Haskell types (also: we can't)
tcErrToProto :: TypeClassError -> Either P.InternalError P.TypeClassError
tcErrToProto = \case
  unknownClass@(UnknownClass _ si) ->
    Right $
      defMessage
        & P.unknownClass
          .~ ( defMessage
                & P.msg .~ mkMsg unknownClass
                & P.sourceInfo .~ toProto si
             )
  refNotFound@(LocalTyRefNotFound _ _ si) ->
    Right $
      defMessage
        & P.refNotFound
          .~ ( defMessage
                & P.msg .~ mkMsg refNotFound
                & P.sourceInfo .~ toProto si
             )
  scCycles@(SuperclassCycleDetected _) ->
    Right $
      defMessage
        & P.superclassCycle .~ (defMessage & P.msg .~ mkMsg scCycles)
  failedToSolve@(FailedToSolveConstraints _ _ _ si) ->
    Right $
      defMessage
        & P.failedToSolveConstraint
          .~ ( defMessage
                & P.msg .~ mkMsg failedToSolve
                & P.sourceInfo .~ toProto si
             )
  malformedDef@(MalformedTyDef _ _ si) ->
    Right $
      defMessage
        & P.malformedTyDef
          .~ ( defMessage
                & P.msg .~ mkMsg malformedDef
                & P.sourceInfo .~ toProto si
             )
  badInst@(BadInstance _ si) ->
    Right $
      defMessage
        & P.malformedTyDef
          .~ ( defMessage
                & P.msg .~ mkMsg badInst
                & P.sourceInfo .~ toProto si
             )
  internal -> Left $ defMessage & P.msg .~ mkMsg internal
