module LambdaBuffers.Compiler.TypeClassCheck (runCheck, runCheck') where

import Control.Lens ((&), (.~), (^.))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Compiler.MiniLog qualified as ML
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.MiniLog (runSolve)
import LambdaBuffers.Compiler.TypeClassCheck.RuleSet (buildRuleSet)
import LambdaBuffers.Compiler.TypeClassCheck.SuperclassCycleCheck qualified as Super
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

runCheck :: PC.CompilerInput -> Either P.CompilerError ()
runCheck = fst . runCheck'

runCheck' :: PC.CompilerInput -> (Either P.CompilerError (), Map PC.ModuleName String)
runCheck' ci = case runSuperClassCycleCheck ci of
  Left err -> (Left err, mempty)
  Right _ -> runConstraintsCheck ci

-- | Determines if type classes form a hierarchichal relation (no cycles).
runSuperClassCycleCheck :: PC.CompilerInput -> Either P.CompilerError ()
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
runConstraintsCheck :: PC.CompilerInput -> (Either P.CompilerError (), Map PC.ModuleName String)
runConstraintsCheck ci =
  let (err, minilogs) = foldr go (memptyErr, mempty) (ci ^. #modules)
   in if err == memptyErr
        then (Right (), minilogs)
        else (Left err, minilogs)
  where
    go m (errs, minilogs) = case buildRuleSet ci m of
      Left err -> (err `mappendErrs` errs, minilogs)
      Right (clauses, goals) ->
        let minilogs' = Map.insert (m ^. #moduleName) (ML.showClauses clauses) minilogs
         in case runSolve (m ^. #moduleName) clauses goals of
              Left err -> (err `mappendErrs` errs, minilogs')
              Right () -> (errs, minilogs')

-- TODO(bladyjoker): This is quite convenient, implement as Semigroup/Monoid and figure out how to use it properly.
mappendErrs :: P.CompilerError -> P.CompilerError -> P.CompilerError
mappendErrs l r =
  defMessage
    & P.protoParseErrors .~ l ^. P.protoParseErrors <> r ^. P.protoParseErrors
    & P.namingErrors .~ l ^. P.namingErrors <> r ^. P.namingErrors
    & P.kindCheckErrors .~ l ^. P.kindCheckErrors <> r ^. P.kindCheckErrors
    & P.tyClassCheckErrors .~ l ^. P.tyClassCheckErrors <> r ^. P.tyClassCheckErrors
    & P.internalErrors .~ l ^. P.internalErrors <> r ^. P.internalErrors

memptyErr :: P.CompilerError
memptyErr = defMessage
