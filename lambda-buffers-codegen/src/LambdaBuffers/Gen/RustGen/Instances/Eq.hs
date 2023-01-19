{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}

module LambdaBuffers.Gen.RustGen.Instances.Eq where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Map as M

import Prettyprinter

import LambdaBuffers.Gen.PP
import LambdaBuffers.Common.Types
import LambdaBuffers.Common.SourceTy (TyDef)
import LambdaBuffers.Resolve.Derive (runDerive)
import LambdaBuffers.Gen.RustGen.RustTy
import LambdaBuffers.Gen.Generator
import LambdaBuffers.Resolve.Rules

--for testing

eq :: Class l
eq = Class "Eq" []

eqScope :: [Instance l]
eqScope = [
   C eq Int :<= []
 , C eq Bool :<= []
 , C eq String :<= []
 , C eq (List _x) :<= [C eq _x]
 , C eq (Maybe _x) :<= [C eq _x]

 -- These are structural rules that don't correspond to any particular existing generated code
 , C eq Nil :<= [] -- I'm not sure whether this really has any meaning
 , C eq (_x :* _xs) :<= [C eq _x, C eq _xs]
 , C eq (_l := _x) :<= [C eq _x]
 , C eq (RecP _xs) :<= [C eq _xs]
 , C eq (ProdP _xs) :<= [C eq _xs]
 , C eq (SumP _xs) :<= [C eq _xs]
 ]

eqTyNoVars :: Instance l
eqTyNoVars = C eq (Sum _name Nil _body) :<= [C eq _body]

fieldName :: Parser Rust c () (Doc ())
fieldName = do
  (Name n := _) <- match (_l := _x)
  pure . pretty $ n

eqTyNoVarsGen :: InstanceGen Rust
eqTyNoVarsGen = do
  Sum (Name tName) Nil body <- match (Sum _name Nil _body)
  case body of

    SumP ((Name cstr := RecP fields) :* Nil) -> do
      funBody <- hcat . punctuate " && " <$> result (someP genFieldEq) fields
      let partialEq =  impl "PartialEq" cstr $ method2 "eq" "bool" funBody
      pure $ partialEq </> eqInst cstr

    SumP (Name cstr := ProdP args :* Nil) -> do
      let counted = zipWith const ([0..] :: [Int]) . unsafeToList $ args
          funBody = hcat
                    . punctuate " && "
                    . for counted
                    $ \(pretty -> ix) -> "self" <.> ix <+> "==" <+> "other" <.> ix
          partialEq = impl "PartialEq" cstr $ method2 "eq" "bool" funBody
      pure $ partialEq </> eqInst cstr

    SumP cstrs -> do
      branches <-  result (someP $ goEnum tName) cstrs
      let partialEq = impl "PartialEq" tName $ method2 "eq" "bool" $ rMatchExp "self" branches
      pure $ partialEq </> eqInst tName

    _ -> undefined
 where
   failMatch = rWildCase "false"

   eqInst :: Text -> Doc a
   eqInst nm = "impl Eq for" <+> pretty nm <+> "{}"

   goEnum :: Text -> Parser Rust c () (Doc ())
   goEnum tName = goNullary <|> goRecord  <|> goProduct
     where
       qualify :: Text  -> Doc a
       qualify d = pretty tName <> "::" <> pretty d

       goNullary = do
         (Name n :=  Nil) <- match (_l := _x)
         pure $ rCase (qualify n)
                   $  rMatchExp "other" [rCase (qualify n) "true", failMatch]

       goProduct = goNullary <|> do
         (Name n := ProdP args') <- match (_l := _x)
         let argVars = zipWith const ['a'..] $ unsafeToList args'
             argMatches = parens . hcat . punctuate " && " $ map (\x -> pretty x <+> "==" <+> pretty x <> "1") argVars
             selfArgs  = rTuple  $ map pretty argVars
             otherArgs = rTuple  $ map ((<> "1") . pretty)  argVars
         pure $ rCase (qualify n <> selfArgs) $ rMatchExp "other" [
                  rCase (qualify n <> otherArgs) argMatches,
                  failMatch]

       goRecord = do
         (Name n := RecP fields') <- match (_l := _x)
         fieldNames <- result (someP fieldName) fields'
         let fields = zip fieldNames ['a'..]
             vars   = map snd fields
             fieldMatches = braces . hcat . punctuate " && " $ map (\x -> pretty x <+> "==" <+> pretty x <> "1") vars
             selfFields  = rBraces . vcat . punctuate "," $ map (\x -> fst x <> ":" <+> pretty (snd x)) fields
             otherFields = rBraces . vcat . punctuate "," $ map (\x -> fst x <> ":" <+> pretty (snd x) <> "1") fields
         pure $  rCase (qualify n <> selfFields) $ rMatchExp "other" [
                    rCase (qualify n <> otherFields) fieldMatches,
                    failMatch ]

   genFieldEq :: Parser Rust c () (Doc ())
   genFieldEq = do
     (Name label' := _) <- match (_l := _x)
     let label = pretty label'
     pure $ "self" <.> label <+> "==" <+> "other" <.> label

eqGen :: M.Map (Instance Rust) (InstanceGen Rust)
eqGen = M.fromList [(eqTyNoVars,eqTyNoVarsGen)]


genTypesAndEqInstances :: (DSL Rust ~ Doc ()) => [TyDef] -> IO ()
genTypesAndEqInstances defs = case mapM (parse' @Rust  rType' . defToPat) defs of
  Left err     -> print err
  Right tyDefs -> case traverse (runDerive eqGen eqScope .  (\t -> C eq t :<= [])) defs' of
    Left err -> print err
    Right instances ->
      let final = foldr (<//>) "" (tyDefs <> concat instances)
      in print final
 where
   defs' = map defToPat defs
