module Test.LambdaBuffers.Compiler.Gen (genCompilerInput) where

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
import Proto.Compiler (ClassName, CompilerInput, ConstrName, Kind, Kind'KindRef (Kind'KIND_REF_TYPE), Module, ModuleName, ModuleNamePart, SourceInfo, Sum, Sum'Constructor, Ty, TyAbs, TyArg, TyBody, TyDef, TyName, VarName)
import Proto.Compiler_Fields (argKind, argName, column, constrName, constructors, fields, file, foreignTyRef, kindArrow, kindRef, left, localTyRef, moduleName, modules, name, ntuple, parts, posFrom, posTo, right, row, sourceInfo, tyAbs, tyApp, tyArgs, tyBody, tyFunc, tyName, tyRef, tyVar, typeDefs, varName)
import Proto.Compiler_Fields qualified as P
import Test.LambdaBuffers.Compiler.Gen.Utils (distribute, indexBy, nesetOf, setOf, vecOf)
import Test.QuickCheck qualified as QC (arbitraryPrintableChar)
import Test.QuickCheck.Gen qualified as QC

-- | Upper bound on various generators
limit :: Int
limit = 5

-- | Names
genAlphaNum :: QC.Gen Char
genAlphaNum = QC.oneof [QC.chooseEnum ('a', 'z'), QC.chooseEnum ('A', 'Z'), QC.chooseEnum ('0', '9')]

genUpperCamelCase :: Int -> QC.Gen Text
genUpperCamelCase len = do
  h <- QC.chooseEnum ('A', 'Z')
  t <- QC.vectorOf len genAlphaNum
  return $ Text.pack $ h : t

genModuleNamePart :: QC.Gen ModuleNamePart
genModuleNamePart = do
  mnp <- genUpperCamelCase 10
  return $ defMessage & name .~ mnp

genModuleName :: QC.Gen ModuleName
genModuleName = do
  ps <- QC.chooseInt (1, limit) >>= vecOf genModuleNamePart
  return $ defMessage & parts .~ ps

genTyName :: QC.Gen TyName
genTyName = do
  n <- genUpperCamelCase 10
  return $ defMessage & name .~ n

_genClassName :: QC.Gen ClassName
_genClassName = do
  n <- genUpperCamelCase 10
  return $ defMessage & name .~ n

genConstrName :: QC.Gen ConstrName
genConstrName = do
  n <- genUpperCamelCase 10
  return $ defMessage & name .~ n

genVarName :: QC.Gen VarName
genVarName = do
  h <- QC.chooseEnum ('a', 'z')
  t <- QC.vectorOf 4 (QC.chooseEnum ('a', 'z'))
  return $ defMessage & name .~ Text.pack (h : t)

starKind :: Kind
starKind = defMessage & kindRef .~ Kind'KIND_REF_TYPE

kindOf :: TyAbs -> Kind
kindOf tyabs = case tyabs ^. tyArgs of
  [] -> starKind
  (a : args) ->
    defMessage
      & kindArrow . left .~ (a ^. argKind)
      & kindArrow . right .~ kindOf (tyabs & tyArgs .~ args)

genTyArg :: VarName -> QC.Gen TyArg
genTyArg vn = do
  return $
    defMessage
      & argName .~ vn
      & argKind .~ starKind -- TODO(bladyjoker): Gen arbitrary kinds.

genSum :: TyDefs -> Set TyArg -> NESet ConstrName -> QC.Gen Sum
genSum tydefs args ctorNs = do
  let (ctorN :| ctorNs') = NESet.toList ctorNs
  ctorNs'' <- QC.sublistOf (toList ctorNs')
  ctors <- for (ctorN :| ctorNs'') (genConstructor tydefs args)
  return $ defMessage & constructors .~ toList ctors

genTy :: Kind -> TyDefs -> Set TyArg -> QC.Gen Ty
genTy kind tydefs tyargs =
  QC.oneof $
    NESet.withNonEmpty [] (genTyVar kind) tyargs
      <> genTyRef kind tydefs
      <> genTyApp kind tydefs tyargs

genTyRef :: Kind -> TyDefs -> [QC.Gen Ty]
genTyRef kind tydefs = case [tyd | tyd <- Map.toList tydefs, kindOf (snd tyd ^. tyAbs) == kind] of
  [] -> []
  tyds ->
    [ do
        tydef <- QC.elements tyds
        case fst tydef of
          Left (mn, tyn) ->
            return $
              defMessage
                & tyRef . foreignTyRef . moduleName .~ mn
                & tyRef . foreignTyRef . tyName .~ tyn
          Right tyn -> return $ defMessage & tyRef . localTyRef . tyName .~ tyn
    ]

genTyVar :: Kind -> NESet TyArg -> [QC.Gen Ty]
genTyVar kind args = case [tyarg | tyarg <- toList args, tyarg ^. argKind == kind] of
  [] -> []
  tyargs ->
    [ do
        tyarg <- QC.elements tyargs
        return $ defMessage & tyVar . varName .~ (tyarg ^. argName)
    ]

genTyApp :: Kind -> TyDefs -> Set TyArg -> [QC.Gen Ty]
genTyApp kind tydefs args =
  let kindFunc =
        defMessage
          & kindArrow . left .~ starKind -- TODO(bladyjoker): Generalize
          & kindArrow . right .~ kind
   in case [tyd | tyd <- toList tydefs, kindOf (tyd ^. tyAbs) == kindFunc] of
        [] -> []
        _ ->
          [ do
              tyfunc <- genTy kindFunc tydefs args
              tyarg <- genTy starKind tydefs args -- TODO(bladyjoker): Generalize
              return $
                defMessage
                  & tyApp . tyFunc .~ tyfunc
                  & tyApp . tyArgs .~ [tyarg] -- TODO(bladyjoker): Generate list arguments
          ]

genConstructor :: TyDefs -> Set TyArg -> ConstrName -> QC.Gen Sum'Constructor
genConstructor tydefs args cn = do
  tys <- QC.chooseInt (0, limit) >>= vecOf (genTy starKind tydefs args)
  return $
    defMessage
      & constrName .~ cn
      & P.product . ntuple . fields .~ tys

genTyBodySum :: TyDefs -> Set TyArg -> NESet ConstrName -> QC.Gen TyBody
genTyBodySum tydefs args ctors = do
  b <- genSum tydefs args ctors
  return $ defMessage & P.sum .~ b

genTyBodyOpaque :: QC.Gen TyBody
genTyBodyOpaque = return $ defMessage & P.opaque .~ defMessage

genTyBody :: TyDefs -> Set TyArg -> NESet ConstrName -> QC.Gen TyBody
genTyBody tydefs args ctorNs =
  QC.oneof $
    [ genTyBodyOpaque
    ]
      -- Gen TyBody'Sum only if there's some TyDefs and TyArgs available
      <> [genTyBodySum tydefs args ctorNs | not (tydefs == mempty && args == mempty)]

genTyAbs :: TyDefs -> NESet ConstrName -> QC.Gen TyAbs
genTyAbs tydefs ctorNs = do
  vns <-
    if tydefs == mempty
      then return mempty
      else QC.chooseInt (0, limit) >>= setOf genVarName
  args <- for (Set.toList vns) genTyArg
  body <- genTyBody tydefs (Set.fromList args) ctorNs
  return $
    defMessage
      & tyArgs .~ toList args
      & tyBody .~ body

type TyDefs = Map (Either (ModuleName, TyName) TyName) TyDef

genTyDef :: TyDefs -> TyName -> NESet ConstrName -> QC.Gen TyDef
genTyDef tydefs tyn ctors = do
  tyabs <- genTyAbs tydefs ctors
  withSourceInfo $
    defMessage
      & tyName .~ tyn
      & tyAbs .~ tyabs

genModule :: Map ModuleName Module -> ModuleName -> QC.Gen Module
genModule availableMods mn = do
  tyNs <- QC.chooseInt (0, limit) >>= nesetOf genTyName
  ctorNs <- QC.chooseInt (length tyNs, length tyNs * limit) >>= nesetOf genConstrName
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
      & moduleName .~ mn
      & typeDefs .~ ([tydef | (n, tydef) <- Map.toList tydefs, isRight n])
  where
    collectTyDefs :: Map ModuleName Module -> TyDefs
    collectTyDefs mods =
      snd
        <$> indexBy
          (\(m, tydef) -> Left (m ^. moduleName, tydef ^. tyName))
          [(m, tydef) | m <- toList mods, tydef <- m ^. typeDefs]

genCompilerInput :: QC.Gen CompilerInput
genCompilerInput = do
  mns <- QC.chooseInt (0, limit) >>= setOf genModuleName
  ms <-
    foldM
      ( \availableMods mn -> do
          m <- genModule availableMods mn
          return $ Map.insert mn m availableMods
      )
      mempty
      (toList mns)
  return $ defMessage & modules .~ toList ms

-- | Utils
withSourceInfo :: HasField a "sourceInfo" SourceInfo => a -> QC.Gen a
withSourceInfo msg = do
  f <- Text.pack <$> vecOf QC.arbitraryPrintableChar 10
  i <- QC.chooseInt (0, Int.maxBound)
  let pos =
        defMessage
          & row .~ fromIntegral i
          & column .~ fromIntegral i
  return $
    msg
      & sourceInfo . file .~ f
      & sourceInfo . posFrom .~ pos
      & sourceInfo . posTo .~ pos
