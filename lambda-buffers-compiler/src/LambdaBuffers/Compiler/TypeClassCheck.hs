{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Compiler.TypeClassCheck (
  detectSuperclassCycles', -- for testing
  detectSuperclassCycles,
  validateTypeClasses,
) where

import Control.Monad (void)
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Map.Internal (traverseWithKey)
import Data.Set qualified as S
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P
import LambdaBuffers.Compiler.TypeClass.Pretty (spaced, (<//>))
import LambdaBuffers.Compiler.TypeClass.Rules (ClassRef, Instance)
import LambdaBuffers.Compiler.TypeClass.Validate.Utils (
  ModuleBuilder (mbInstances),
  TypeClassError (CouldntSolveConstraints, SuperclassCycleDetected),
  checkDerive,
  mkBuilders,
  mkClassInfos,
  toClassMap,
 )
import Prettyprinter (
  Doc,
  Pretty (pretty),
  indent,
  line,
  punctuate,
  vcat,
  (<+>),
 )

detectSuperclassCycles' :: P.CompilerInput -> [[ClassRef]]
detectSuperclassCycles' (P.CompilerInput modules) =
  detectCycles . toClassMap . concat . M.elems . mkClassInfos $ modules
  where
    detectCycles :: forall k. Ord k => M.Map k [k] -> [[k]]
    detectCycles m = concatMap (detect []) (M.keys m)
      where
        detect :: [k] -> k -> [[k]]
        detect visited x = case M.lookup x m of
          Nothing -> []
          Just xs ->
            if x `elem` visited
              then [x : visited]
              else concatMap (detect (x : visited)) xs

detectSuperclassCycles :: P.CompilerInput -> Either TypeClassError ()
detectSuperclassCycles ci = case detectSuperclassCycles' ci of
  [] -> Right ()
  xs -> Left $ SuperclassCycleDetected xs

-- ModuleBuilder is suitable codegen input,
-- and is (relatively) computationally expensive to
-- construct, so we return it here if successful.
validateTypeClasses' :: P.CompilerInput -> Either TypeClassError (M.Map P.ModuleName ModuleBuilder)
validateTypeClasses' ci = do
  detectSuperclassCycles ci
  moduleBuilders <- mkBuilders ci
  void $ traverseWithKey runDeriveCheck moduleBuilders
  pure moduleBuilders
  where
    runDeriveCheck :: P.ModuleName -> ModuleBuilder -> Either TypeClassError ()
    runDeriveCheck mn mb = mconcat <$> traverse go (S.toList $ mbInstances mb)
      where
        go :: Instance -> Either TypeClassError ()
        go i =
          checkDerive mn mb i >>= \case
            [] -> pure ()
            xs -> Left $ CouldntSolveConstraints mn xs i

-- maybe use Control.Exception? Tho if we're not gonna catch it i guess this is fine
validateTypeClasses :: P.CompilerInput -> IO (M.Map P.ModuleName ModuleBuilder)
validateTypeClasses ci = case validateTypeClasses' ci of
  Left err -> print (spaced $ pretty err) >> error "\nCompilation aborted due to TypeClass Error"
  Right mbs -> print (prettyBuilders mbs) >> pure mbs

prettyBuilders :: forall a. M.Map P.ModuleName ModuleBuilder -> Doc a
prettyBuilders = spaced . vcat . punctuate line . map (uncurry go) . M.toList
  where
    go :: P.ModuleName -> ModuleBuilder -> Doc a
    go mn mb =
      "MODULE"
        <+> pretty mn
        <//> indent 2 (pretty mb)
