module LambdaBuffers.Compiler.TypeClassCheck (runCheck) where

import Control.Lens ((&), (.~), (^.))
import Data.Foldable (for_)
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.MiniLog (runSolve)
import LambdaBuffers.Compiler.TypeClassCheck.RuleSet (buildRuleSet)
import LambdaBuffers.Compiler.TypeClassCheck.SuperclassCycleCheck qualified as Super
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

runCheck :: PC.CompilerInput -> Either P.CompilerError ()
runCheck ci = do
  runSuperClassCycleCheck ci
  runConstraintsCheck ci

-- | Determines if type classes form a hierarchichal relation (no cycles).
runSuperClassCycleCheck :: PC.CompilerInput -> Either P.CompilerError ()
runSuperClassCycleCheck ci = case Super.runCheck ci of
  Left errs -> Left $ defMessage & P.tyClassCheckErrors .~ errs
  Right _ -> Right ()

{- | Runs checks on type class constraints.
 Traverses the modules and builds a rule set for each. For any
 `PC.InstanceClause` and `PC.Derive` encountered, turn them into a query goal
 and try to solve it.
 TODO(bladyjoker): Don't err eagerly, rather collect and report back errors for each module.
-}
runConstraintsCheck :: PC.CompilerInput -> Either P.CompilerError ()
runConstraintsCheck ci =
  for_
    (ci ^. #modules)
    ( \m -> do
        (clauses, goals) <- buildRuleSet ci m
        runSolve (m ^. #moduleName) clauses goals
    )
