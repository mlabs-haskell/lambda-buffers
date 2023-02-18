{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Compiler.TypeClassCheck (detectSuperclassCycles, detectSuperclassCycles', runDeriveCheck, validateTypeClasses) where

import Control.Lens.Combinators (view)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Data.Generics.Labels ()
import Data.List (foldl')
import Data.Map (traverseWithKey)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import LambdaBuffers.Compiler.ProtoCompat.Types (
  ClassDef (),
  ForeignClassRef (ForeignClassRef),
  LocalClassRef (LocalClassRef),
  TyClassRef (ForeignCI, LocalCI),
 )
import LambdaBuffers.Compiler.TypeClass.Pretty (spaced, (<//>))
import LambdaBuffers.Compiler.TypeClass.Utils (
  Instance,
  ModuleBuilder (mbInstances),
  TypeClassError (FailedToSolveConstraints),
  checkInstance,
  mkBuilders,
 )
import LambdaBuffers.Compiler.TypeClass.Validate (checkDerive)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  hcat,
  indent,
  line,
  punctuate,
  vcat,
  (<+>),
 )

data ClassInfo = ClassInfo {ciName :: Text, ciSupers :: [Text]}
  deriving stock (Show, Eq, Ord)

detectSuperclassCycles' :: [ClassDef] -> [[Text]]
detectSuperclassCycles' = detectCycles . mkClassGraph . map defToClassInfo
  where
    defToClassInfo :: ClassDef -> ClassInfo
    defToClassInfo cd =
      ClassInfo (cd ^. #className . #name) $
        map (extractName . view #classRef) (cd ^. #supers)
      where
        extractName = \case
          LocalCI (LocalClassRef nm _) -> nm ^. #name
          ForeignCI (ForeignClassRef nm _ _) -> nm ^. #name

    mkClassGraph :: [ClassInfo] -> M.Map Text [Text]
    mkClassGraph = foldl' (\acc (ClassInfo nm sups) -> M.insert nm sups acc) M.empty

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

detectSuperclassCycles :: forall a. [ClassDef] -> Maybe (Doc a)
detectSuperclassCycles cds = case detectSuperclassCycles' cds of
  [] -> Nothing
  xs ->
    Just $
      "Error: Superclass cycle(s) detected"
        <> line
        <> indent 2 (vcat $ map format xs)
  where
    format :: [Text] -> Doc a
    format = hcat . punctuate " => " . map pretty

runDeriveCheck :: P.ModuleName -> ModuleBuilder -> Either TypeClassError ()
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
validateTypeClasses' :: P.CompilerInput -> Either TypeClassError (M.Map P.ModuleName ModuleBuilder)
validateTypeClasses' ci = do
  -- detectSuperclassCycles ci
  moduleBuilders <- mkBuilders ci
  void $ traverseWithKey runDeriveCheck moduleBuilders
  pure moduleBuilders

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
