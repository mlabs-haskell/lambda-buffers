module LambdaBuffers.Compiler.TypeClassCheck.RuleSet (buildRuleSet) where

import Control.Lens ((^.))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import LambdaBuffers.Compiler.MiniLog qualified as ML
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.Errors (importNotFoundError)
import LambdaBuffers.Compiler.TypeClassCheck.MiniLog (Clause, Term, mkDeriveRule, mkInstanceRule, mkQuery, mkStructuralRules)
import Proto.Compiler qualified as P

buildRuleSet' ::
  PC.TyDefs ->
  PC.ClassRels ->
  PC.CompilerInput ->
  Set (PC.InfoLess PC.ModuleName) ->
  PC.Module ->
  Either P.CompilerError ([Clause], [Term], Set (PC.InfoLess PC.ModuleName))
buildRuleSet' _ _ _ visited m | infoLessMn m `Set.member` visited = return ([], [], visited)
buildRuleSet' tyDefs classRels ci visited m = do
  let structuralRules = concat $ mkStructuralRules (m ^. #moduleName) <$> toList (m ^. #classDefs)

  instanceClauses <-
    for
      (m ^. #instances)
      (mkInstanceRule (m ^. #moduleName) classRels)

  derivedClauses <-
    for
      (m ^. #derives)
      (mkDeriveRule (m ^. #moduleName) tyDefs classRels)

  -- Fold over imports, accumulating clauses while keeping and passing the
  -- `visited` as to not perform a duplicate load.
  -- TODO(bladyjoker): Rewrite in State monad.
  (importClauses, visited') <-
    foldrM
      ( \imp (clauses, vs) -> do
          case Map.lookup (PC.mkInfoLess imp) (ci ^. #modules) of
            Nothing -> Left $ importNotFoundError (m ^. #moduleName) imp
            Just impMod -> do
              (clauses', _, vs') <- buildRuleSet' tyDefs classRels ci vs impMod
              return (clauses ++ clauses', vs')
      )
      (mempty, infoLessMn m `Set.insert` visited)
      (toList $ m ^. #imports)

  -- Take the head of each user defined rule and turn it into a query goal to try and solve.
  let goals = mkQuery . ML.clauseHead <$> instanceClauses <> derivedClauses

  return (instanceClauses <> derivedClauses <> structuralRules <> importClauses, goals, visited')

infoLessMn :: PC.Module -> PC.InfoLess PC.ModuleName
infoLessMn m = PC.mkInfoLess (m ^. #moduleName)

-- | Builds a Lambda Buffers rules set and query goals given the available type class definitions, instance clauses and derive statements in the compiler input.
buildRuleSet :: PC.CompilerInput -> PC.Module -> Either P.CompilerError ([Clause], [Term])
buildRuleSet ci m =
  let tyDefs = PC.indexTyDefs ci
      classRels = PC.indexClassRelations ci
   in (\(cls, gs, _) -> (cls, gs)) <$> buildRuleSet' tyDefs classRels ci mempty m
