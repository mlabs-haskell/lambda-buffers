{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Compiler.TypeClassCheck (runDeriveCheck, runCheck) where

import Control.Lens ((&), (.~))
import Control.Monad (void)
import Data.Generics.Labels ()
import Data.Map (traverseWithKey)
import Data.Map qualified as M
import Data.ProtoLens (Message (defMessage))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.Pretty (spaced, (<//>))
import LambdaBuffers.Compiler.TypeClassCheck.SuperclassCycleCheck qualified as Super
import LambdaBuffers.Compiler.TypeClassCheck.Utils (
  Instance,
  ModuleBuilder (mbInstances),
  TypeClassError (FailedToSolveConstraints),
  checkInstance,
  mkBuilders,
 )
import LambdaBuffers.Compiler.TypeClassCheck.Validate (checkDerive)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  indent,
  line,
  punctuate,
  vcat,
  (<+>),
 )
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

data ClassInfo = ClassInfo {ciName :: Text, ciSupers :: [Text]}
  deriving stock (Show, Eq, Ord)

runDeriveCheck :: PC.InfoLess PC.ModuleName -> ModuleBuilder -> Either TypeClassError ()
runDeriveCheck mn mb = mconcat <$> traverse go (S.toList $ mbInstances mb)
  where
    go :: Instance -> Either TypeClassError ()
    go i =
      checkInstance i
        >> checkDerive mn mb i
        >>= \case
          [] -> pure ()
          xs -> Left $ FailedToSolveConstraints mn xs i

runDeriveCheck' :: PC.CompilerInput -> Either TypeClassError (M.Map (PC.InfoLess PC.ModuleName) ModuleBuilder)
runDeriveCheck' ci = do
  moduleBuilders <- mkBuilders ci
  void $ traverseWithKey runDeriveCheck moduleBuilders
  pure moduleBuilders

runCheck :: PC.CompilerInput -> Maybe P.CompilerError
runCheck ci = case Super.runCheck ci of
  Left errs -> Just $ defMessage & P.tyClassCheckErrors .~ errs
  Right () -> case runDeriveCheck' ci of
    Left err -> Just $ defMessage & P.internalErrors .~ [defMessage & P.msg .~ ("TODO(bladyjoker): Use proper errors" <> (Text.pack . show $ err))]
    Right _ -> Nothing

_validateTypeClasses :: PC.CompilerInput -> IO (M.Map (PC.InfoLess PC.ModuleName) ModuleBuilder)
_validateTypeClasses ci = case runDeriveCheck' ci of
  Left err -> print (spaced $ pretty err) >> error "\nCompilation aborted due to TypeClass Error"
  Right mbs -> print (_prettyBuilders mbs) >> pure mbs

_prettyBuilders :: forall a. M.Map (PC.InfoLess PC.ModuleName) ModuleBuilder -> Doc a
_prettyBuilders = spaced . vcat . punctuate line . map (uncurry go) . M.toList
  where
    go :: PC.InfoLess PC.ModuleName -> ModuleBuilder -> Doc a
    go mn mb =
      "MODULE"
        <+> PC.withInfoLess mn pretty
        <//> indent 2 (pretty mb)
