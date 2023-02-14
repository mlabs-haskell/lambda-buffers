module Test.LambdaBuffers.Compiler.Gen (genCompilerInput) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad.Reader (MonadTrans (lift), ReaderT, replicateM)
import Control.Monad.State (StateT)
import Data.List qualified as List
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Proto.Compiler (ClassName, CompilerInput, ConstrName, Kind'KindRef (Kind'KIND_REF_TYPE), Module, ModuleName, ModuleNamePart, Sum, Sum'Constructor, Ty, TyAbs, TyArg, TyBody, TyDef, TyName, VarName)
import Proto.Compiler_Fields (argKind, argName, constrName, constructors, fields, kindRef, moduleName, modules, name, ntuple, parts, tyAbs, tyArgs, tyBody, tyName, tyVar, typeDefs, varName)
import Proto.Compiler_Fields qualified as P
import Test.QuickCheck qualified as QC (Gen, chooseEnum, chooseInt, elements, oneof, vectorOf)

data GenProtoCtx
data GenProtoState

type GenProto a = ReaderT GenProtoCtx (StateT GenProtoState QC.Gen) a

vecOf :: forall {a}. GenProto a -> Int -> GenProto [a]
vecOf g n = replicateM n g

chooseInt :: (Int, Int) -> GenProto Int
chooseInt r = lift . lift $ QC.chooseInt r

-- | Names
genAlphaNum :: QC.Gen Char
genAlphaNum = QC.oneof [QC.chooseEnum ('a', 'z'), QC.chooseEnum ('A', 'Z'), QC.chooseEnum ('0', '9')]

genUpperCamelCase :: Int -> QC.Gen Text
genUpperCamelCase len = do
  h <- QC.chooseEnum ('A', 'Z')
  t <- QC.vectorOf len genAlphaNum
  return $ Text.pack $ h : t

genModuleNamePart :: GenProto ModuleNamePart
genModuleNamePart = do
  mnp <- lift . lift $ genUpperCamelCase 10
  return $ defMessage & name .~ mnp

genModuleName :: GenProto ModuleName
genModuleName = do
  ps <- chooseInt (1, 5) >>= vecOf genModuleNamePart
  return $ defMessage & parts .~ ps

genTyName :: GenProto TyName
genTyName = do
  n <- lift . lift $ genUpperCamelCase 10
  return $ defMessage & name .~ n

_genClassName :: GenProto ClassName
_genClassName = do
  n <- lift . lift $ genUpperCamelCase 10
  return $ defMessage & name .~ n

genConstrName :: GenProto ConstrName
genConstrName = do
  n <- lift . lift $ genUpperCamelCase 10
  return $ defMessage & name .~ n

genVarName :: GenProto VarName
genVarName = do
  h <- lift . lift $ QC.chooseEnum ('a', 'z')
  t <- lift . lift $ QC.vectorOf 4 (QC.chooseEnum ('a', 'z'))
  return $ defMessage & name .~ Text.pack (h : t)

genTyArg :: VarName -> GenProto TyArg
genTyArg vn = do
  return $
    defMessage
      & argName .~ vn
      & argKind . kindRef .~ Kind'KIND_REF_TYPE -- TODO(bladyjoker): Gen arbitrary kinds.

genSum :: [TyArg] -> GenProto Sum
genSum args = do
  cns <- chooseInt (1, 10) >>= vecOf genConstrName
  ctors <- for (List.nub cns) (genConstructor args)
  return $ defMessage & constructors .~ ctors

-- TODO(bladyjoker): Add TyRef, TyApp etc.
genTy :: [TyArg] -> GenProto Ty
genTy args = do
  ar <- lift . lift $ QC.elements args
  return $ defMessage & tyVar . varName .~ (ar ^. argName)

genConstructor :: [TyArg] -> ConstrName -> GenProto Sum'Constructor
genConstructor args cn = do
  tys <- chooseInt (1, 10) >>= vecOf (genTy args)
  return $
    defMessage
      & constrName .~ cn
      & P.product . ntuple . fields .~ tys

-- TODO(bladyjoker): Add Opaque.
genTyBody :: [TyArg] -> GenProto TyBody
genTyBody args = do
  b <- genSum args
  return $ defMessage & P.sum .~ b

genTyAbs :: GenProto TyAbs
genTyAbs = do
  vns <- chooseInt (1, 10) >>= vecOf genVarName
  args <- for (List.nub vns) genTyArg
  body <- genTyBody args
  return $
    defMessage
      & tyArgs .~ args
      & tyBody .~ body

genTyDef :: TyName -> GenProto TyDef
genTyDef tn = do
  tyabs <- genTyAbs
  return $
    defMessage
      & tyName .~ tn
      & tyAbs .~ tyabs

genModule :: ModuleName -> GenProto Module
genModule mn = do
  tns <- chooseInt (1, 100) >>= vecOf genTyName
  tydefs <- for (List.nub tns) genTyDef
  return $
    defMessage
      & moduleName .~ mn
      & typeDefs .~ tydefs

genCompilerInput :: GenProto CompilerInput
genCompilerInput = do
  mns <- chooseInt (1, 100) >>= vecOf genModuleName
  ms <- for (List.nub mns) genModule
  return $ defMessage & modules .~ ms
