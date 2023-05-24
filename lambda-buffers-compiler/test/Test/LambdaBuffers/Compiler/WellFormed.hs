module Test.LambdaBuffers.Compiler.WellFormed (genCompilerInput) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (foldM)
import Data.Either (isRight)
import Data.Foldable (Foldable (toList))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Field (HasField)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import GHC.Enum qualified as Int
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as HR
import Proto.Compiler qualified as Compiler
import Proto.Compiler_Fields qualified as Compiler
import Proto.Lang qualified as Lang
import Proto.Lang_Fields qualified as Lang
import Test.LambdaBuffers.Compiler.Utils (distribute, indexBy)

-- | Default constant range used in various generators
defRange :: H.Range Int
defRange = HR.constant 0 5

-- | Names
genAlphaNum :: H.Gen Char
genAlphaNum = H.alphaNum

genUpperCamelCase :: H.Gen Text
genUpperCamelCase = do
  h <- H.upper
  t <- H.list defRange genAlphaNum
  return $ Text.pack $ h : t

genModuleNamePart :: H.Gen Lang.ModuleNamePart
genModuleNamePart = do
  mnp <- genUpperCamelCase
  return $ defMessage & Lang.name .~ mnp

genModuleName :: H.Gen Lang.ModuleName
genModuleName = do
  ps <- H.list defRange genModuleNamePart
  return $ defMessage & Lang.parts .~ ps

genTyName :: H.Gen Lang.TyName
genTyName = do
  n <- genUpperCamelCase
  return $ defMessage & Lang.name .~ n

_genClassName :: H.Gen Lang.ClassName
_genClassName = do
  n <- genUpperCamelCase
  return $ defMessage & Lang.name .~ n

genConstrName :: H.Gen Lang.ConstrName
genConstrName = do
  n <- genUpperCamelCase
  return $ defMessage & Lang.name .~ n

genVarName :: H.Gen Lang.VarName
genVarName = do
  h <- H.lower
  t <- H.list defRange H.lower
  return $ defMessage & Lang.name .~ Text.pack (h : t)

starKind :: Lang.Kind
starKind = defMessage & Lang.kindRef .~ Lang.Kind'KIND_REF_TYPE

kindOf :: Lang.TyAbs -> Lang.Kind
kindOf tyabs = case tyabs ^. Lang.tyArgs of
  [] -> starKind
  (a : args) ->
    defMessage
      & Lang.kindArrow . Lang.left .~ (a ^. Lang.argKind)
      & Lang.kindArrow . Lang.right .~ kindOf (tyabs & Lang.tyArgs .~ args)

genTyArg :: Lang.VarName -> H.Gen Lang.TyArg
genTyArg vn = do
  return $
    defMessage
      & Lang.argName .~ vn
      & Lang.argKind .~ starKind -- TODO(bladyjoker): Gen arbitrary kinds.

genSum :: TyDefs -> Set Lang.TyArg -> NESet Lang.ConstrName -> H.Gen Lang.Sum
genSum tydefs args ctorNs = do
  let (ctorN :| ctorNs') = NESet.toList ctorNs
  ctorNs'' <- H.subsequence ctorNs'
  ctors <- for (ctorN :| ctorNs'') (genConstructor tydefs args)
  return $ defMessage & Lang.constructors .~ toList ctors

genTy :: Lang.Kind -> TyDefs -> Set Lang.TyArg -> H.Gen Lang.Ty
genTy kind tydefs tyargs =
  H.choice $
    NESet.withNonEmpty [] (genTyVar kind) tyargs
      <> genTyRef kind tydefs
      <> genTyApp kind tydefs tyargs

genTyRef :: Lang.Kind -> TyDefs -> [H.Gen Lang.Ty]
genTyRef kind tydefs = case [tyd | tyd <- Map.toList tydefs, kindOf (snd tyd ^. Lang.tyAbs) == kind] of
  [] -> []
  tyds ->
    [ do
        tydef <- H.element tyds
        case fst tydef of
          Left (mn, tyn) ->
            return $
              defMessage
                & Lang.tyRef . Lang.foreignTyRef . Lang.moduleName .~ mn
                & Lang.tyRef . Lang.foreignTyRef . Lang.tyName .~ tyn
          Right tyn -> return $ defMessage & Lang.tyRef . Lang.localTyRef . Lang.tyName .~ tyn
    ]

genTyVar :: Lang.Kind -> NESet Lang.TyArg -> [H.Gen Lang.Ty]
genTyVar kind args = case [tyarg | tyarg <- toList args, tyarg ^. Lang.argKind == kind] of
  [] -> []
  tyargs ->
    [ do
        tyarg <- H.element tyargs
        return $ defMessage & Lang.tyVar . Lang.varName .~ (tyarg ^. Lang.argName)
    ]

genTyApp :: Lang.Kind -> TyDefs -> Set Lang.TyArg -> [H.Gen Lang.Ty]
genTyApp kind tydefs args =
  let kindFunc =
        defMessage
          & Lang.kindArrow . Lang.left .~ starKind -- TODO(bladyjoker): Generalize
          & Lang.kindArrow . Lang.right .~ kind
   in case [tyd | tyd <- toList tydefs, kindOf (tyd ^. Lang.tyAbs) == kindFunc] of
        [] -> []
        _ ->
          [ do
              tyfunc <- genTy kindFunc tydefs args
              tyarg <- genTy starKind tydefs args -- TODO(bladyjoker): Generalize
              return $
                defMessage
                  & Lang.tyApp . Lang.tyFunc .~ tyfunc
                  & Lang.tyApp . Lang.tyArgs .~ [tyarg] -- TODO(bladyjoker): Generate list arguments
          ]

genConstructor :: TyDefs -> Set Lang.TyArg -> Lang.ConstrName -> H.Gen Lang.Sum'Constructor
genConstructor tydefs args cn = do
  tys <- H.list defRange (genTy starKind tydefs args)
  return $
    defMessage
      & Lang.constrName .~ cn
      & Lang.product . Lang.fields .~ tys

genTyBodySum :: TyDefs -> Set Lang.TyArg -> NESet Lang.ConstrName -> H.Gen Lang.TyBody
genTyBodySum tydefs args ctors = do
  b <- genSum tydefs args ctors
  return $ defMessage & Lang.sum .~ b

genTyBodyOpaque :: H.Gen Lang.TyBody
genTyBodyOpaque = return $ defMessage & Lang.opaque .~ defMessage

genTyBody :: TyDefs -> Set Lang.TyArg -> NESet Lang.ConstrName -> H.Gen Lang.TyBody
genTyBody tydefs args ctorNs =
  H.choice $
    [ genTyBodyOpaque
    ]
      -- Gen TyBody'Sum only if there's some TyDefs and TyArgs available
      <> [genTyBodySum tydefs args ctorNs | not (tydefs == mempty && args == mempty)]

genTyAbs :: TyDefs -> NESet Lang.ConstrName -> H.Gen Lang.TyAbs
genTyAbs tydefs ctorNs = do
  vns <-
    if tydefs == mempty
      then return mempty
      else H.set defRange genVarName
  args <- for (Set.toList vns) genTyArg
  body <- genTyBody tydefs (Set.fromList args) ctorNs
  return $
    defMessage
      & Lang.tyArgs .~ toList args
      & Lang.tyBody .~ body

type TyDefs = Map (Either (Lang.ModuleName, Lang.TyName) Lang.TyName) Lang.TyDef

genTyDef :: TyDefs -> Lang.TyName -> NESet Lang.ConstrName -> H.Gen Lang.TyDef
genTyDef tydefs tyn ctors = do
  tyabs <- genTyAbs tydefs ctors
  withSourceInfo $
    defMessage
      & Lang.tyName .~ tyn
      & Lang.tyAbs .~ tyabs

genModule :: Map Lang.ModuleName Lang.Module -> Lang.ModuleName -> H.Gen Lang.Module
genModule availableMods mn = do
  tyNs <- NESet.fromList <$> H.nonEmpty defRange genTyName
  ctorNs <- H.set (HR.constant (length tyNs) (length tyNs * 10)) genConstrName
  tyNsWithCtorNs <- Map.map NESet.fromList <$> distribute (toList ctorNs) (NESet.toSet tyNs)
  let foreignTyDefs = collectTyDefs availableMods
  tydefs <-
    foldM
      ( \allTyDefs (tyN, ctorNs') -> do
          tydef <- genTyDef allTyDefs tyN ctorNs'
          return $ Map.insert (Right tyN) tydef allTyDefs
      )
      foreignTyDefs
      (Map.toList tyNsWithCtorNs)
  return $
    defMessage
      & Lang.moduleName .~ mn
      & Lang.typeDefs .~ ([tydef | (n, tydef) <- Map.toList tydefs, isRight n])
  where
    collectTyDefs :: Map Lang.ModuleName Lang.Module -> TyDefs
    collectTyDefs mods =
      snd
        <$> indexBy
          (\(m, tydef) -> Left (m ^. Lang.moduleName, tydef ^. Lang.tyName))
          [(m, tydef) | m <- toList mods, tydef <- m ^. Lang.typeDefs]

genCompilerInput :: H.Gen Compiler.Input
genCompilerInput = do
  mns <- H.set defRange genModuleName
  ms <-
    foldM
      ( \availableMods mn -> do
          m <- genModule availableMods mn
          return $ Map.insert mn m availableMods
      )
      mempty
      (toList mns)
  return $ defMessage & Compiler.modules .~ toList ms

-- | Utils
withSourceInfo :: HasField a "sourceInfo" Lang.SourceInfo => a -> H.Gen a
withSourceInfo msg = do
  f <- Text.pack <$> H.list (HR.constant 1 10) H.unicodeAll
  i <- H.int (HR.constant 0 Int.maxBound)
  let pos =
        defMessage
          & Lang.row .~ fromIntegral i
          & Lang.column .~ fromIntegral i
  return $
    msg
      & Lang.sourceInfo . Lang.file .~ f
      & Lang.sourceInfo . Lang.posFrom .~ pos
      & Lang.sourceInfo . Lang.posTo .~ pos
