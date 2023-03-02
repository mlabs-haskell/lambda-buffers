{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Compiler.TypeClassCheck (runDeriveCheck, validateTypeClasses) where

import Control.Monad (void)
import Data.Generics.Labels ()
import Data.Map (traverseWithKey)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.TypeClassCheck.Pretty (spaced, (<//>))
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

data ClassInfo = ClassInfo {ciName :: Text, ciSupers :: [Text]}
  deriving stock (Show, Eq, Ord)

runDeriveCheck :: P.InfoLess P.ModuleName -> ModuleBuilder -> Either TypeClassError ()
runDeriveCheck mn mb = mconcat <$> traverse go (S.toList $ mbInstances mb)
  where
    go :: Instance -> Either TypeClassError ()
    go i =
      checkInstance i
        >> checkDerive mn mb i
        >>= \case
          [] -> pure ()
          xs -> Left $ FailedToSolveConstraints mn xs i

-- ModuleBuilder is suitable codegen input,
-- and is (relatively) computationally expensive to
-- construct, so we return it here if successful.
validateTypeClasses' :: P.CompilerInput -> Either TypeClassError (M.Map (P.InfoLess P.ModuleName) ModuleBuilder)
validateTypeClasses' ci = do
  -- detectSuperclassCycles ci
  moduleBuilders <- mkBuilders ci
  void $ traverseWithKey runDeriveCheck moduleBuilders
  pure moduleBuilders

-- maybe use Control.Exception? Tho if we're not gonna catch it i guess this is fine
validateTypeClasses :: P.CompilerInput -> IO (M.Map (P.InfoLess P.ModuleName) ModuleBuilder)
validateTypeClasses ci = case validateTypeClasses' ci of
  Left err -> print (spaced $ pretty err) >> error "\nCompilation aborted due to TypeClass Error"
  Right mbs -> print (prettyBuilders mbs) >> pure mbs

prettyBuilders :: forall a. M.Map (P.InfoLess P.ModuleName) ModuleBuilder -> Doc a
prettyBuilders = spaced . vcat . punctuate line . map (uncurry go) . M.toList
  where
    go :: P.InfoLess P.ModuleName -> ModuleBuilder -> Doc a
    go mn mb =
      "MODULE"
        <+> P.withInfoLess mn pretty
        <//> indent 2 (pretty mb)
