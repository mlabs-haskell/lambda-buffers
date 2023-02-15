module Test.LambdaBuffers.Compiler.Gen (genCompilerInput) where

import Control.Lens ((&), (.~), (^.))
import Data.Foldable (Foldable (toList))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Proto.Compiler (ClassName, CompilerInput, ConstrName, Kind'KindRef (Kind'KIND_REF_TYPE), Module, ModuleName, ModuleNamePart, Sum, Sum'Constructor, Ty, TyAbs, TyArg, TyBody, TyDef, TyName, TyRef, VarName)
import Proto.Compiler_Fields (argKind, argName, constrName, constructors, fields, kindRef, moduleName, modules, name, ntuple, parts, tyAbs, tyArgs, tyBody, tyName, tyRef, tyVar, typeDefs, varName)
import Proto.Compiler_Fields qualified as P
import Test.QuickCheck qualified as QC (Gen, chooseEnum, chooseInt, elements, oneof, vectorOf)

vecOf :: forall {a}. QC.Gen a -> Int -> QC.Gen [a]
vecOf = flip QC.vectorOf

limit :: Int
limit = 4

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
genTy :: [TyRef] -> [TyArg] -> QC.Gen Ty
genTy (r : refs) (a : args) = QC.oneof [genTyVar (a :| args), genTyRef (r :| refs)]
genTy [] (a : args) = QC.oneof [genTyVar (a :| args)]
genTy (r : refs) [] = QC.oneof [genTyRef (r :| refs)]
genTy _ _ = error "TODO(bladyjoker): Not yet implemented"

genTyRef :: NonEmpty TyRef -> QC.Gen Ty
genTyRef refs = do
  r <- QC.elements (toList refs)
  return $ defMessage & tyRef .~ r

genTyVar :: NonEmpty TyArg -> QC.Gen Ty
genTyVar args = do
  ar <- QC.elements (toList args)
  return $ defMessage & tyVar . varName .~ (ar ^. argName)

genConstructor :: [TyArg] -> ConstrName -> QC.Gen Sum'Constructor
genConstructor args cn = do
  tys <- QC.chooseInt (1, limit) >>= vecOf (genTy [] args)
  return $
    defMessage
      & constrName .~ cn
      & P.product . ntuple . fields .~ tys

genTyBody :: [TyArg] -> QC.Gen TyBody
genTyBody args = QC.oneof [genTyBodyOpaque, genTyBodySum args]

genTyBodySum :: [TyArg] -> QC.Gen TyBody
genTyBodySum args = do
  b <- genSum args
  return $ defMessage & P.sum .~ b

genTyBodyOpaque :: QC.Gen TyBody
genTyBodyOpaque = return $ defMessage & P.opaque .~ defMessage

genTyAbs :: QC.Gen TyAbs
genTyAbs = do
  -- TODO(bladyjoker): Allow empty args
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
