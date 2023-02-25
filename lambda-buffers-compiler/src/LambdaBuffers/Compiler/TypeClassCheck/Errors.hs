module LambdaBuffers.Compiler.TypeClassCheck.Errors (
  type Instance,
  TypeClassError (UnknownClass, UnknownModule, MissingModuleInstances, MissingModuleScope, ClassNotFoundInModule, LocalTyRefNotFound, SuperclassCycleDetected, FailedToSolveConstraints, MalformedTyDef, BadInstance),
  BasicConditionViolation (TyConInContext, OverlapDetected, OnlyTyVarsInHead),
) where

import GHC.Generics (Generic)

import Data.Text qualified as T

import LambdaBuffers.Compiler.TypeClassCheck.Rules (
  Constraint,
  FQClassName,
  Rule,
 )

import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ModuleName,
  SourceInfo,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Pat (Exp, Pat)

import LambdaBuffers.Compiler.TypeClassCheck.Pretty (pointies, (<///>))
import LambdaBuffers.Compiler.TypeClassCheck.Solve (Overlap (Overlap))
import Prettyprinter (
  Pretty (pretty),
  hcat,
  indent,
  line,
  nest,
  punctuate,
  vcat,
  (<+>),
 )

type Instance = Rule Pat

{- Some of these are perfunctory & used to keep functions total.
-}
data TypeClassError
  = UnknownClass FQClassName P.SourceInfo -- this might need split into two, investigate further
  | UnknownModule P.ModuleName -- internal, no sourceInfo (doesn't make sense)
  | MissingModuleInstances P.ModuleName -- internal, no sourceInfo (doesn't make sense)
  | MissingModuleScope P.ModuleName -- internal, no sourceinfo (doesn't make sense)
  | ClassNotFoundInModule Text [Text] -- internal-ish? this one's weird. there's not really one place in the source that triggers it. come back to it later
  | LocalTyRefNotFound T.Text P.ModuleName P.SourceInfo -- SI is the instance clause that triggered the tyref lookup
  | SuperclassCycleDetected [[FQClassName]] -- No sourceinfo, it's not very useful due to the nature of cycles
  | FailedToSolveConstraints P.ModuleName [Constraint Exp] Instance P.SourceInfo -- SI is the instance clause that triggered the subgoal that failed (subgoal itself may not exist anywhere in the source)
  | MalformedTyDef P.ModuleName Exp P.SourceInfo -- SI is the BODY of the exp
  | BadInstance BasicConditionViolation P.SourceInfo -- SI is the source of the rule that triggered the violation. This might be weird
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
