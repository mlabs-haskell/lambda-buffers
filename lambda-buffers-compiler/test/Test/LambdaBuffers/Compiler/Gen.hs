module Test.LambdaBuffers.Compiler.Gen (genCompilerInput) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (replicateM)
import Data.List qualified as List
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Proto.Compiler (ClassName, CompilerInput, ConstrName, Kind'KindRef (Kind'KIND_REF_TYPE), Module, ModuleName, ModuleNamePart, Sum, Sum'Constructor, Ty, TyAbs, TyArg, TyBody, TyDef, TyName, VarName)
import Proto.Compiler_Fields (argKind, argName, constrName, constructors, fields, kindRef, moduleName, modules, name, ntuple, parts, tyAbs, tyArgs, tyBody, tyName, tyVar, typeDefs, varName)
import Proto.Compiler_Fields qualified as P
import Test.QuickCheck qualified as QC (Gen, chooseEnum, chooseInt, elements, oneof, vectorOf)

vecOf :: forall {a}. QC.Gen a -> Int -> QC.Gen [a]
vecOf g n = replicateM n g

limit :: Int
limit = 10

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

genTyArg :: VarName -> QC.Gen TyArg
genTyArg vn = do
  return $
    defMessage
      & argName .~ vn
      & argKind . kindRef .~ Kind'KIND_REF_TYPE -- TODO(bladyjoker): QC.Gen arbitrary kinds.

genSum :: [TyArg] -> QC.Gen Sum
genSum args = do
  cns <- QC.chooseInt (1, limit) >>= vecOf genConstrName
  ctors <- for (List.nub cns) (genConstructor args)
  return $ defMessage & constructors .~ ctors

-- TODO(bladyjoker): Add TyRef, TyApp etc.
genTy :: [TyArg] -> QC.Gen Ty
genTy args = do
  ar <- QC.elements args
  return $ defMessage & tyVar . varName .~ (ar ^. argName)

genConstructor :: [TyArg] -> ConstrName -> QC.Gen Sum'Constructor
genConstructor args cn = do
  tys <- QC.chooseInt (1, limit) >>= vecOf (genTy args)
  return $
    defMessage
      & constrName .~ cn
      & P.product . ntuple . fields .~ tys

-- TODO(bladyjoker): Add Opaque.
genTyBody :: [TyArg] -> QC.Gen TyBody
genTyBody args = do
  b <- genSum args
  return $ defMessage & P.sum .~ b

genTyAbs :: QC.Gen TyAbs
genTyAbs = do
  vns <- QC.chooseInt (1, limit) >>= vecOf genVarName
  args <- for (List.nub vns) genTyArg
  body <- genTyBody args
  return $
    defMessage
      & tyArgs .~ args
      & tyBody .~ body

genTyDef :: TyName -> QC.Gen TyDef
genTyDef tn = do
  tyabs <- genTyAbs
  return $
    defMessage
      & tyName .~ tn
      & tyAbs .~ tyabs

genModule :: ModuleName -> QC.Gen Module
genModule mn = do
  tns <- QC.chooseInt (1, limit) >>= vecOf genTyName
  tydefs <- for (List.nub tns) genTyDef
  return $
    defMessage
      & moduleName .~ mn
      & typeDefs .~ tydefs

genCompilerInput :: QC.Gen CompilerInput
genCompilerInput = do
  mns <- QC.chooseInt (1, limit) >>= vecOf genModuleName
  ms <- for (List.nub mns) genModule
  return $ defMessage & modules .~ ms
