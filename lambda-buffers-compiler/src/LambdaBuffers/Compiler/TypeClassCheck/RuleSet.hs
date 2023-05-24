module LambdaBuffers.Compiler.TypeClassCheck.RuleSet (buildRules) where

import Control.Lens ((^.))
import Data.Foldable (Foldable (toList), foldrM)
import Data.Map (Map)
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

buildRulesForModule ::
  (PC.TyDefs, PC.ClassRels, PC.CompilerInput) ->
  Set (PC.InfoLess PC.ModuleName) ->
  PC.Module ->
  Either P.Error ([Clause], [Term], Set (PC.InfoLess PC.ModuleName))
buildRulesForModule _ visited m | infoLessMn m `Set.member` visited = return ([], [], visited)
buildRulesForModule ctx@(tyDefs, classRels, ci) visited m = do
  let structuralRules = concatMap (mkStructuralRules (m ^. #moduleName)) (toList (m ^. #classDefs))

  -- ClassRef scope is imports + locally defined.
  let classRelsInScope =
        Map.filterWithKey
          ( \(mn, _cn) _ ->
              mn `Map.member` (m ^. #imports)
                || mn == PC.mkInfoLess (m ^. #moduleName)
          )
          classRels

  instanceClauses <-
    for
      (m ^. #instances)
      (mkInstanceRule (m ^. #moduleName) classRelsInScope)

  derivedClauses <-
    for
      (m ^. #derives)
      (mkDeriveRule (m ^. #moduleName) tyDefs classRelsInScope)

  -- Fold over imports, accumulating clauses while keeping and passing the
  -- `visited` so we don't import duplicate rules.
  -- TODO(bladyjoker): Rewrite in State monad.
  -- NOTE(bladyjoker): This corresponds to Haskell rule scoping semantics, is this what we want?
  (importClauses, visited') <-
    foldrM
      ( \imp (clauses, vs) -> do
          case Map.lookup (PC.mkInfoLess imp) (ci ^. #modules) of
            Nothing -> Left $ importNotFoundError (m ^. #moduleName) imp
            Just impMod -> do
              (clauses', _, vs') <- buildRulesForModule ctx vs impMod
              return (clauses ++ clauses', vs')
      )
      (mempty, infoLessMn m `Set.insert` visited)
      (toList $ m ^. #imports)

  -- Take the head of each user defined rule and turn it into a query goal to try and solve.
  let goals = mkQuery . ML.clauseHead <$> instanceClauses <> derivedClauses

  return (instanceClauses <> derivedClauses <> structuralRules <> importClauses, goals, visited')

infoLessMn :: PC.Module -> PC.InfoLess PC.ModuleName
infoLessMn m = PC.mkInfoLess (m ^. #moduleName)

-- | Builds Lambda Buffers rules and query goals given the available type class definitions, instance clauses and derive statements in the compiler input.
buildRules :: PC.CompilerInput -> Map PC.ModuleName (Either P.Error ([Clause], [Term]))
buildRules ci =
  let tyDefs = PC.indexTyDefs ci
      classRels = PC.indexClassRelations ci
   in foldr
        ( \m acc ->
            Map.insert
              (m ^. #moduleName)
              ((\(clauses, goals, _) -> (clauses, goals)) <$> buildRulesForModule (tyDefs, classRels, ci) mempty m)
              acc
        )
        mempty
        (ci ^. #modules)
