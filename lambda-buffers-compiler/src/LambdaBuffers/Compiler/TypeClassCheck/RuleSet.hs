module LambdaBuffers.Compiler.TypeClassCheck.RuleSet (buildRuleSet) where

import Control.Lens ((&), (.~), (^.))
import Data.Foldable (Foldable (toList))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Traversable (for)
import LambdaBuffers.Compiler.MiniLog qualified as ML
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.MiniLog (Clause, Term, mkDeriveRule, mkInstanceRule, mkQuery, mkStructuralRules)
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

-- | Builds a Lambda Buffers rules set and query goals given the available type class definitions, instance clauses and derive statements.
buildRuleSet' :: PC.TyDefs -> PC.ClassRels -> PC.CompilerInput -> PC.Module -> Either P.CompilerError ([Clause], [Term])
buildRuleSet' tyDefs classRels ci m = do
  let structuralRules = concat $ mkStructuralRules (m ^. #moduleName) <$> toList (m ^. #classDefs)

  instanceClauses <-
    for
      (m ^. #instances)
      (mkInstanceRule (m ^. #moduleName) classRels)

  derivedClauses <-
    for
      (m ^. #derive)
      (mkDeriveRule (m ^. #moduleName) tyDefs classRels)

  -- WARN(bladyjoker): Assumes the ImportCycleDetection has been run before.
  -- TODO(bladyjoker): Implement ImportCycleDetection.
  importClauses <-
    concat
      <$> for
        (toList $ m ^. #imports)
        ( \mn -> do
            case Map.lookup (PC.mkInfoLess mn) (ci ^. #modules) of
              Nothing -> Left $ defMessage & P.internalErrors .~ [defMessage & P.msg .~ "Imported module not found"]
              Just impMod -> fst <$> buildRuleSet' tyDefs classRels ci impMod
        )
  let goals = mkQuery . ML.clauseHead <$> instanceClauses <> derivedClauses
  return (instanceClauses <> derivedClauses <> structuralRules <> importClauses, goals)

buildRuleSet :: PC.CompilerInput -> PC.Module -> Either P.CompilerError ([Clause], [Term])
buildRuleSet ci m =
  let tyDefs = PC.indexTyDefs ci
      classRels = PC.indexClassRelations ci
   in buildRuleSet' tyDefs classRels ci m
