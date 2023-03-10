{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | MiniLog encoding of LambdaBuffers Type Class rules.
module LambdaBuffers.Compiler.MiniLog.LambdaBuffers (Term, Clause, Atom (..), Funct (..), mkInstanceRule, mkDeriveRule) where

import Control.Lens (view, (^.))
import Data.Map (Map)
import Data.Map.Ordered qualified as OMap
import LambdaBuffers.Compiler.MiniLog ((@), (@<=))
import LambdaBuffers.Compiler.MiniLog qualified as ML
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Proto.Compiler qualified as P

type Term = ML.Term Funct Atom

type Clause = ML.Clause Funct Atom

data Atom
  = AFieldName (PC.InfoLess PC.FieldName)
  | AConstrName (PC.InfoLess PC.ConstrName)
  | AVarName (PC.InfoLess PC.VarName)
  | ATyName PC.QTyName
  | ANil
  deriving stock (Eq, Ord, Show)

data Funct
  = FSum
  | FTuple
  | FRec
  | FOpaque
  | FApp
  | FAbs
  | FClass PC.QClassName
  | FField
  | FConstructor
  | FListCons
  deriving stock (Eq, Ord, Show)

(@|) :: Term -> Term -> Term
(@|) h t = ML.struct FListCons [h, t]

nilt :: Term
nilt = ML.Atom ANil

tapp :: Term -> Term -> Term
tapp f args = ML.struct FApp [f, args]

tabs :: Term -> Term -> Term
tabs args body = ML.struct FAbs [args, body]

tsum :: Term -> Term
tsum s = ML.struct FSum [s]

ttuple :: Term -> Term
ttuple p = ML.struct FTuple [p]

trec :: Term -> Term
trec r = ML.struct FRec [r]

tfield :: Term -> Term -> Term
tfield fn ty = ML.struct FField [fn, ty]

tctor :: Term -> Term -> Term
tctor cn p = ML.struct FConstructor [cn, p]

topaque :: Term
topaque = ML.struct FOpaque []

tclass :: PC.QClassName -> Term -> Term
tclass qcn a = ML.struct (FClass qcn) [a]

mkSumRule :: PC.QClassName -> Clause
mkSumRule qcn =
  let tc = tclass qcn
   in tc (tsum (tctor ("C" @) ("P" @) @| ("T" @)))
        @<= [ tc ("P" @)
            , tc $ tsum ("T" @)
            ]

mkTupleRule :: PC.QClassName -> Clause
mkTupleRule qcn =
  let tc = tclass qcn
   in tc (ttuple (("H" @) @| ("T" @)))
        @<= [ tc ("H" @)
            , tc $ ttuple ("T" @)
            ]

mkRecRule :: PC.QClassName -> Clause
mkRecRule qcn =
  let tc = tclass qcn
   in tc (trec (tfield ("Fn" @) ("FTy" @) @| ("Fs" @)))
        @<= [ tc ("FTy" @)
            , tc $ trec ("Fs" @)
            ]

mkVoidRule :: PC.QClassName -> Clause
mkVoidRule qcn = let tc = tclass qcn in tc (tsum nilt) @<= []

mkUnitRuleTuple :: PC.QClassName -> Clause
mkUnitRuleTuple qcn = let tc = tclass qcn in tc (ttuple nilt) @<= []

mkUnitRuleRec :: PC.QClassName -> Clause
mkUnitRuleRec qcn = let tc = tclass qcn in tc (trec nilt) @<= []

mkOpaqueRule :: PC.QClassName -> Clause
mkOpaqueRule qcn = let tc = tclass qcn in tc topaque @<= []

-- TODO: Use Derive instance of Constraint.
mkDeriveRule :: PC.ModuleName -> Map PC.QTyName PC.TyDef -> PC.ClassDef -> PC.Constraint -> Either P.CompilerError Clause
mkDeriveRule mn tyds cldef cr =
  let ty = E.fromTy $ cr ^. #argument
      tc = tclass (qualifyClassRef mn $ cr ^. #classRef)
      tsupers = tclass . qualifyClassRef mn . view #classRef <$> cldef ^. #supers
   in case E.runEval' mn tyds ty of
        Left err -> Left err
        Right ty' -> Right $ tc (tyT mn ty) @<= (tc (tyT mn ty') : (($ tyT mn ty) <$> tsupers))

mkInstanceRule :: PC.ModuleName -> PC.ClassDef -> PC.InstanceClause -> Clause
mkInstanceRule mn cldef inst =
  let thead = tyT mn . E.fromTy $ inst ^. #head
      tc = tclass (qualifyClassRef mn $ inst ^. #classRef)
      superConstraints = ($ thead) . tclass . qualifyClassRef mn . view #classRef <$> cldef ^. #supers
      constraints = (\c -> tclass (qualifyClassRef mn (c ^. #classRef)) (tyT mn . E.fromTy $ c ^. #argument)) <$> inst ^. #constraints
   in tc thead @<= (constraints ++ superConstraints)

tyT :: PC.ModuleName -> E.Ty -> Term
tyT mn (E.TyRef tr) = ML.Atom . ATyName . qualifyTyRef mn $ tr
tyT _ (E.TyVar tv) = ML.Var (tv ^. #varName . #name)
tyT _ (E.TyOpaque _) = topaque
tyT mn (E.TyAbs args body _) = tabs (foldr (\(vn, _) t -> (ML.Atom . AVarName $ vn) @| t) nilt (OMap.assocs args)) (tyT mn body)
tyT mn (E.TyApp tf args _) = tapp (tyT mn tf) (foldr (\ty t -> tyT mn ty @| t) nilt args)
tyT mn (E.TySum ctors _) = tsum $ foldr (\(cn, cp) t -> tctor (ML.Atom . AConstrName $ cn) (tyT mn cp) @| t) nilt (OMap.assocs ctors)
tyT mn (E.TyTuple fields _) = ttuple $ foldr (\fty t -> tyT mn fty @| t) nilt fields
tyT mn (E.TyRecord fields _) = trec $ foldr (\(fn, fty) t -> tfield (ML.Atom . AFieldName $ fn) (tyT mn fty) @| t) nilt (OMap.assocs fields)

qualifyClassRef :: PC.ModuleName -> PC.TyClassRef -> PC.QClassName
qualifyClassRef _ (PC.ForeignCI fcr) = (PC.mkInfoLess $ fcr ^. #moduleName, PC.mkInfoLess $ fcr ^. #className)
qualifyClassRef mn (PC.LocalCI lcr) = (PC.mkInfoLess mn, PC.mkInfoLess $ lcr ^. #className)

qualifyTyRef :: PC.ModuleName -> PC.TyRef -> PC.QTyName
qualifyTyRef _ (PC.ForeignI fr) = (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
qualifyTyRef mn (PC.LocalI lr) = (PC.mkInfoLess mn, PC.mkInfoLess $ lr ^. #tyName)
