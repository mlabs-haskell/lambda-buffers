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
import Proto.Compiler (ClassName, CompilerInput, ConstrName, Kind, Kind'KindRef (Kind'KIND_REF_TYPE), Module, ModuleName, ModuleNamePart, SourceInfo, Sum, Sum'Constructor, Ty, TyAbs, TyArg, TyBody, TyDef, TyName, VarName)
import Proto.Compiler_Fields (argKind, argName, column, constrName, constructors, fields, file, foreignTyRef, kindArrow, kindRef, left, localTyRef, moduleName, modules, name, ntuple, parts, posFrom, posTo, right, row, sourceInfo, tyAbs, tyApp, tyArgs, tyBody, tyFunc, tyName, tyRef, tyVar, typeDefs, varName)
import Proto.Compiler_Fields qualified as P
import Test.LambdaBuffers.Compiler.Utils (distribute, indexBy)


-- | Default constant range
defRange = HR.constant lowerBound upperBound
-- or defSize

-- | Upper bound on various generators
upperBound :: Int
upperBound = 5

-- | Lower bound on various generators
lowerBound :: Int
lowerBound = 1


-- | Names
genAlphaNum :: H.Gen Char
genAlphaNum = H.alphaNum

genUpperCamelCase :: H.Gen Text
genUpperCamelCase = do
  h <- H.upper
  t <- H.list defRange genAlphaNum
  return $ Text.pack $ h : t

genModuleNamePart :: H.Gen ModuleNamePart
genModuleNamePart = do
  mnp <- genUpperCamelCase
  return $ defMessage & name .~ mnp

genModuleName :: H.Gen ModuleName
genModuleName = do
  ps <- H.list defRange genModuleNamePart
  return $ defMessage & parts .~ ps

genTyName :: H.Gen TyName
genTyName = do
  n <- genUpperCamelCase
  return $ defMessage & name .~ n

_genClassName :: H.Gen ClassName
_genClassName = do
  n <- genUpperCamelCase
  return $ defMessage & name .~ n

genConstrName :: H.Gen ConstrName
genConstrName = do
  n <- genUpperCamelCase
  return $ defMessage & name .~ n

genVarName :: H.Gen VarName
genVarName = do
  h <- H.lower
  t <- H.list defRange H.lower
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

genTyArg :: VarName -> H.Gen TyArg
genTyArg vn = do
  return $
    defMessage
      & argName .~ vn
      & argKind .~ starKind -- TODO(bladyjoker): Gen arbitrary kinds.

genSum :: TyDefs -> Set TyArg -> NESet ConstrName -> H.Gen Sum
genSum tydefs args ctorNs = do
  let (ctorN :| ctorNs') = NESet.toList ctorNs
  ctorNs'' <- H.subsequence ctorNs'
  ctors <- for (ctorN :| ctorNs'') (genConstructor tydefs args)
  return $ defMessage & constructors .~ toList ctors

genTy :: Kind -> TyDefs -> Set TyArg -> H.Gen Ty
genTy kind tydefs tyargs =
  H.choice $
    NESet.withNonEmpty [] (genTyVar kind) tyargs
      <> genTyRef kind tydefs
      <> genTyApp kind tydefs tyargs

genTyRef :: Kind -> TyDefs -> [H.Gen Ty]
genTyRef kind tydefs = case [tyd | tyd <- Map.toList tydefs, kindOf (snd tyd ^. tyAbs) == kind] of
  [] -> []
  tyds ->
    [ do
        tydef <- H.element tyds
        case fst tydef of
          Left (mn, tyn) ->
            return $
              defMessage
                & tyRef . foreignTyRef . moduleName .~ mn
                & tyRef . foreignTyRef . tyName .~ tyn
          Right tyn -> return $ defMessage & tyRef . localTyRef . tyName .~ tyn
    ]

genTyVar :: Kind -> NESet TyArg -> [H.Gen Ty]
genTyVar kind args = case [tyarg | tyarg <- toList args, tyarg ^. argKind == kind] of
  [] -> []
  tyargs ->
    [ do
        tyarg <- H.element tyargs
        return $ defMessage & tyVar . varName .~ (tyarg ^. argName)
    ]

genTyApp :: Kind -> TyDefs -> Set TyArg -> [H.Gen Ty]
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

genConstructor :: TyDefs -> Set TyArg -> ConstrName -> H.Gen Sum'Constructor
genConstructor tydefs args cn = do
  tys <- H.list (HR.constant 0 limit) (genTy starKind tydefs args)
  return $
    defMessage
      & constrName .~ cn
      & P.product . ntuple . fields .~ tys

genTyBodySum :: TyDefs -> Set TyArg -> NESet ConstrName -> H.Gen TyBody
genTyBodySum tydefs args ctors = do
  b <- genSum tydefs args ctors
  return $ defMessage & P.sum .~ b

genTyBodyOpaque :: H.Gen TyBody
genTyBodyOpaque = return $ defMessage & P.opaque .~ defMessage

genTyBody :: TyDefs -> Set TyArg -> NESet ConstrName -> H.Gen TyBody
genTyBody tydefs args ctorNs =
  H.choice $
    [ genTyBodyOpaque
    ]
      -- Gen TyBody'Sum only if there's some TyDefs and TyArgs available
      <> [genTyBodySum tydefs args ctorNs | not (tydefs == mempty && args == mempty)]

genTyAbs :: TyDefs -> NESet ConstrName -> H.Gen TyAbs
genTyAbs tydefs ctorNs = do
  vns <-
    if tydefs == mempty
      then return mempty
      else H.set (HR.constant 0 limit) genVarName
  args <- for (Set.toList vns) genTyArg
  body <- genTyBody tydefs (Set.fromList args) ctorNs
  return $
    defMessage
      & tyArgs .~ toList args
      & tyBody .~ body

type TyDefs = Map (Either (ModuleName, TyName) TyName) TyDef

genTyDef :: TyDefs -> TyName -> NESet ConstrName -> H.Gen TyDef
genTyDef tydefs tyn ctors = do
  tyabs <- genTyAbs tydefs ctors
  withSourceInfo $
    defMessage
      & tyName .~ tyn
      & tyAbs .~ tyabs

genModule :: Map ModuleName Module -> ModuleName -> H.Gen Module
genModule availableMods mn = do
  tyNs <- NESet.fromList <$> H.nonEmpty (HR.constant 0 limit) genTyName
  ctorNs <- H.set (HR.constant (length tyNs) (length tyNs * limit)) genConstrName
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

genCompilerInput :: H.Gen CompilerInput
genCompilerInput = do
  mns <- H.set (HR.constant 0 limit) genModuleName
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
withSourceInfo :: HasField a "sourceInfo" SourceInfo => a -> H.Gen a
withSourceInfo msg = do
  f <- Text.pack <$> H.list (HR.constant 1 10) H.unicodeAll
  i <- H.int (HR.constant 0 Int.maxBound)
  let pos =
        defMessage
          & row .~ fromIntegral i
          & column .~ fromIntegral i
  return $
    msg
      & sourceInfo . file .~ f
      & sourceInfo . posFrom .~ pos
      & sourceInfo . posTo .~ pos
