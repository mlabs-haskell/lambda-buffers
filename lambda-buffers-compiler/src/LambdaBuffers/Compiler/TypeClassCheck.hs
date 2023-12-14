module LambdaBuffers.Compiler.TypeClassCheck (runCheck, runCheck') where

import Control.Lens ((&), (.~))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.MiniLog.Pretty qualified as ML
import LambdaBuffers.Compiler.TypeClassCheck.MiniLog (Clause, Term, runSolve)
import LambdaBuffers.Compiler.TypeClassCheck.RuleSet (buildRules)
import LambdaBuffers.Compiler.TypeClassCheck.SuperclassCycleCheck qualified as Super
import LambdaBuffers.ProtoCompat qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

runCheck :: PC.CompilerInput -> Either P.Error ()
runCheck = fst . runCheck'

runCheck' :: PC.CompilerInput -> (Either P.Error (), Map FilePath String)
runCheck' ci = case runSuperClassCycleCheck ci of
  Left err -> (Left err, mempty)
  Right _ -> runConstraintsCheck ci

-- | Determines if type classes form a hierarchical relation (no cycles).
runSuperClassCycleCheck :: PC.CompilerInput -> Either P.Error ()
runSuperClassCycleCheck ci = case Super.runCheck ci of
  Left errs -> Left $ defMessage & P.tyClassCheckErrors .~ errs
  Right _ -> Right ()

{- | Runs checks on type class constraints.
 Traverses the modules and builds a rule set for each. For any
 `PC.InstanceClause` and `PC.Derive` encountered, turn the head constraint into
 a query goal and try to solve/evaluate it.

 Errors are collected for each module and provided back to the caller along with
 a map of Prolog rendered MiniLog clauses for each module which can be used to
 inspect the rules if needed.
-}
runConstraintsCheck :: PC.CompilerInput -> (Either P.Error (), Map FilePath String)
runConstraintsCheck ci =
  let (errs, printed) =
        foldr
          solveAndPrint
          (mempty, mempty)
          (Map.toList . buildRules $ ci)
   in if errs == mempty
        then (Right (), printed)
        else (Left errs, printed)

solveAndPrint :: (PC.ModuleName, Either P.Error ([Clause], [Term])) -> (P.Error, Map FilePath String) -> (P.Error, Map FilePath String)
solveAndPrint (mn, errOrClauses) (errs, printed) =
  case errOrClauses of
    Left buildErr -> (buildErr <> errs, printed)
    Right (clauses, goals) ->
      let (fp, prolog) = ML.toPrologModule (modNameToText mn) clauses
          printed' = Map.insert fp prolog printed
       in case runSolve mn clauses goals of
            Left solveErr -> (solveErr <> errs, printed')
            Right _ -> (errs, printed')

modNameToText :: PC.ModuleName -> Text
modNameToText = Text.pack . show . PC.prettyModuleName
