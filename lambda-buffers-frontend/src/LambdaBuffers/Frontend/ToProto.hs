module LambdaBuffers.Frontend.ToProto (toCompilerInput) where

import Control.Lens ((&), (.~))
import Data.ProtoLens (Message (defMessage))
import LambdaBuffers.Frontend.Syntax (
  ConstrName (ConstrName),
  Constructor (Constructor),
  Import,
  Module (Module),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  Product (Product),
  SourceInfo (SourceInfo),
  SourcePos (SourcePos),
  Ty (TyApp, TyRef', TyVar),
  TyArg (TyArg),
  TyBody (Opaque, Sum),
  TyDef (TyDef),
  TyName (TyName),
  TyRef (TyRef),
  VarName (VarName),
 )
import Proto.Compiler (Kind'KindRef (Kind'KIND_REF_TYPE))
import Proto.Compiler qualified as P
import Proto.Compiler_Fields as PF (argKind, argName, column, constrName, constructors, fields, file, kindRef, localTyRef, moduleName, modules, name, ntuple, opaque, parts, posFrom, posTo, product, row, sourceInfo, sum, tyAbs, tyApp, tyArgs, tyBody, tyFunc, tyName, tyRef, tyVar, typeDefs, varName)

toCompilerInput :: [Module SourceInfo] -> P.CompilerInput
toCompilerInput ms = defMessage & modules .~ (toModule <$> ms)

toModule :: Module SourceInfo -> P.Module
toModule (Module mn imps tyds info) =
  defMessage
    & moduleName .~ toModuleName mn
    & typeDefs .~ (toTypeDef imps <$> tyds)
    & sourceInfo .~ toSourceInfo info

toTypeDef :: [Import SourceInfo] -> TyDef SourceInfo -> P.TyDef
toTypeDef _ (TyDef tn args body info) =
  defMessage
    & tyName .~ toTyName tn
    & ( case args of
          [] -> tyBody .~ toTyBody body
          (a : as) -> tyAbs .~ toTyAbs a as body
      )
    & sourceInfo .~ toSourceInfo info

toTyBody :: TyBody SourceInfo -> P.TyBody
toTyBody (Sum cs info) =
  defMessage
    & PF.sum . constructors .~ (toConstructor <$> cs)
    & PF.sum . sourceInfo .~ toSourceInfo info
toTyBody Opaque = defMessage & opaque .~ defMessage

-- TODO(bladyjoker): Add SourceInfo?
toConstructor :: Constructor SourceInfo -> P.Sum'Constructor
toConstructor (Constructor cname prod info) =
  defMessage
    & constrName .~ toConstName cname
    & PF.product .~ toProduct prod

toProduct :: Product SourceInfo -> P.Product
toProduct (Product tys info) =
  defMessage
    & ntuple . fields .~ (toTy <$> tys)
    & ntuple . sourceInfo .~ toSourceInfo info

toTy :: Ty SourceInfo -> P.Ty
toTy (TyVar vn info) =
  defMessage
    & tyVar . varName .~ toVarName vn
    & tyVar . sourceInfo .~ toSourceInfo info
toTy (TyApp ty tys info) =
  defMessage
    & tyApp . tyFunc .~ toTy ty
    & tyApp . tyArgs .~ (toTy <$> tys)
    & tyApp . sourceInfo .~ toSourceInfo info
toTy (TyRef' (TyRef mayModAl tn info) _) =
  defMessage
    & tyRef . localTyRef . tyName .~ toTyName tn
    & tyRef . localTyRef . sourceInfo .~ toSourceInfo info

toVarName :: VarName SourceInfo -> P.VarName
toVarName (VarName n info) =
  defMessage
    & name .~ n
    & sourceInfo .~ toSourceInfo info

toConstName :: ConstrName SourceInfo -> P.ConstrName
toConstName (ConstrName cn info) =
  defMessage
    & name .~ cn
    & sourceInfo .~ toSourceInfo info

-- TODO(bladyjoker): TyAbs needs SourceInfo? Remove it if not.
toTyAbs :: TyArg SourceInfo -> [TyArg SourceInfo] -> TyBody SourceInfo -> P.TyAbs
toTyAbs arg args body =
  defMessage
    & tyArgs .~ (toTyArg <$> arg : args)
    & tyBody .~ toTyBody body

toTyArg :: TyArg SourceInfo -> P.TyArg
toTyArg (TyArg an info) =
  defMessage
    & argName . name .~ an
    & argKind . kindRef .~ Kind'KIND_REF_TYPE
    & sourceInfo .~ toSourceInfo info

toTyName :: TyName SourceInfo -> P.TyName
toTyName (TyName n info) =
  defMessage
    & name .~ n
    & sourceInfo .~ toSourceInfo info

toSourceInfo :: SourceInfo -> P.SourceInfo
toSourceInfo (SourceInfo f fr to) =
  defMessage
    & file .~ f
    & posFrom .~ toSourcePos fr
    & posTo .~ toSourcePos to

toSourcePos :: SourcePos -> P.SourcePosition
toSourcePos (SourcePos r c) =
  defMessage
    & row .~ fromIntegral r
    & column .~ fromIntegral c

toModuleName :: ModuleName SourceInfo -> P.ModuleName
toModuleName (ModuleName ps info) =
  defMessage
    & parts .~ (toModuleNamePart <$> ps)
    & sourceInfo .~ toSourceInfo info

toModuleNamePart :: ModuleNamePart SourceInfo -> P.ModuleNamePart
toModuleNamePart (ModuleNamePart p info) =
  defMessage
    & name .~ p
    & sourceInfo .~ toSourceInfo info
