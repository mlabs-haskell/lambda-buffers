{-# LANGUAGE  OverloadedLabels #-}

module LambdaBuffers.CodeGen where

import LambdaBuffers.CodeGen.Common.Types ( for, Pat )
import LambdaBuffers.CodeGen.Resolve.Solve
import LambdaBuffers.CodeGen.Resolve.Rules
import LambdaBuffers.CodeGen.Gen.Generator
import LambdaBuffers.CodeGen.Common.Compat
import LambdaBuffers.CodeGen.Resolve.Derive
import qualified LambdaBuffers.Common.ProtoCompat as P

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Lens
import Data.Text (Text)
import Control.Monad.Trans.Except (runExceptT)


data ModuleBuilder l = ModuleBuilder {
  mbName      :: P.ModuleName,
  mbDefs      :: [(Pat,P.SourceInfo)], -- Patterns representing types from the module tagged w/ their source location for error reporting
  mbInstances :: Instances l,          -- just the instances from the module
  mbClasses   :: Classes l,            -- just the classes from the module
  mbImports   :: [P.ModuleName],
  mbRefs      :: ForeignRefs           -- will probably need this at some point?
}

type TypeGen e l = Parser l TypeDecl e (DSL l)

types :: [P.TyDef] -> NameSpaced [(Pat,P.SourceInfo)]
types = traverse (\x -> (,x ^. #sourceInfo) <$> defToPat x)

mkModuleBuilder :: forall l. P.CompilerInput -> Either InstanceError [ModuleBuilder l]
mkModuleBuilder (P.CompilerInput inp) = traverse (secondPass . firstPass) inp
  where
    -- FIXME(gnumonik): Ugh this is bad. Two classes defined w/ the same name in diff modules will break things.
    --                  I think the only way to fix this is to change the proto somehow, not sure how atm
    allClasses :: M.Map Text (Class l)
    allClasses  = S.foldl' (\acc c@(Class nm _) -> M.insert nm c acc) M.empty
                  . mconcat
                  . flip evalState M.empty
                  $ traverse (\x -> getClasses (x ^. #classDefs)) inp

    firstPass :: P.Module -> InstanceM  (P.ModuleName,[(Pat,P.SourceInfo)],Instances l,Classes l)
    firstPass (P.Module modNm tDefs cDefs instances _ _) = do
      mbdefs <- lift $ types tDefs
      mbclasses <- lift $ getClasses cDefs
      mbinstances <- getInstances allClasses instances
      pure (modNm,mbdefs,mbinstances,mbclasses)

    secondPass :: InstanceM (P.ModuleName,[(Pat,P.SourceInfo)],Instances l,Classes l)
               -> Either InstanceError (ModuleBuilder l)
    secondPass m = case runState (runExceptT m) M.empty of
      (Left err,_)              -> Left err
      (Right (mn,ds,is,cs),ns)  -> Right $ ModuleBuilder mn ds is cs (deps ns) ns
     where
      deps :: ForeignRefs -> [P.ModuleName]
      deps = S.toList . S.fromList . M.elems . sanitizeRefs
