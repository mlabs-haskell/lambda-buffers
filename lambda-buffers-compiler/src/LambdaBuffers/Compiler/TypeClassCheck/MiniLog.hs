-- | MiniLog encoding of LambdaBuffers Type Class rules.
module LambdaBuffers.Compiler.TypeClassCheck.MiniLog (
  Term,
  Clause,
  Atom (..),
  Funct (..),
  mkInstanceRule,
  mkStructuralRules,
  mkDeriveRule,
  mkQuery,
  runSolve,
) where

import Control.Lens (view, (&), (.~), (^.))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import LambdaBuffers.Compiler.MiniLog ((@), (@<=))
import LambdaBuffers.Compiler.MiniLog qualified as ML
import LambdaBuffers.Compiler.MiniLog.UniFdSolver qualified as ML
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import Proto.Compiler qualified as P
import Proto.Compiler_Fields qualified as P

type Term = ML.Term Funct Atom

type Clause = ML.Clause Funct Atom

data Atom
  = AFieldName (PC.InfoLess PC.FieldName)
  | AConstrName (PC.InfoLess PC.ConstrName)
  | AVarName (PC.InfoLess PC.VarName)
  | ATyName PC.QTyName
  | ANil
  | AText Text
  deriving stock (Eq, Ord)

-- | Used by MiniLog to print as Prolog atoms.
instance Show Atom where
  show (AFieldName n) = PC.withInfoLess n (show . view #name)
  show (AConstrName n) = PC.withInfoLess n (show . view #name)
  show (AVarName n) = PC.withInfoLess n (show . view #name)
  show (ATyName (mn, tn)) = PC.withInfoLess mn (concatMap (show . view #name) . view #parts) <> "." <> PC.withInfoLess tn (show . view #name)
  show ANil = "[]"
  show (AText txt) = show txt

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
  | -- | Used when solving a term that contains a variable.
    FVar
  deriving stock (Eq, Ord)

-- | Used by MiniLog to print as Prolog functors.
instance Show Funct where
  show (FClass (mn, cn)) = PC.withInfoLess mn (concatMap (show . view #name) . view #parts) <> "." <> PC.withInfoLess cn (show . view #name)
  show FSum = "sum"
  show FTuple = "tuple"
  show FRec = "rec"
  show FOpaque = "opaque"
  show FApp = "app"
  show FAbs = "abs"
  show FField = "field"
  show FConstructor = "ctor"
  show FListCons = "|"
  show FVar = "var"

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

tvar :: Term -> Term
tvar vn = ML.struct FVar [vn]

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

mkVarRule :: PC.QClassName -> Clause
mkVarRule qcn = let tc = tclass qcn in tc (tvar ("X" @)) @<= []

mkStructuralRules :: PC.ModuleName -> PC.ClassDef -> [Clause]
mkStructuralRules mn cd =
  let qcn = PC.qualifyClassName mn (cd ^. #className)
   in ($ qcn)
        <$> [ mkVarRule
            , mkUnitRuleRec
            , mkUnitRuleTuple
            , mkVoidRule
            , mkRecRule
            , mkTupleRule
            , mkSumRule
            ]

mkSupersBody :: PC.ModuleName -> PC.ClassRels -> PC.Constraint -> Either P.CompilerError [Term]
mkSupersBody mn clrels cstr =
  let ty = tyToTerm mn . E.fromTy $ cstr ^. #argument
   in case Map.lookup (PC.qualifyClassRef mn $ cstr ^. #classRef) clrels of
        Nothing ->
          Left $
            defMessage
              & P.internalErrors
                .~ [ defMessage & P.msg .~ ("TODO(bladyjoker): UnboundClassRef" <> (Text.pack . show . view #classRef $ cstr))
                   ]
        Just sups -> Right ((`tclass` ty) <$> sups)

termFromConstraint :: PC.ModuleName -> PC.Constraint -> Term
termFromConstraint mn cstr =
  let ty = tyToTerm mn . E.fromTy $ cstr ^. #argument
      tc = tclass (PC.qualifyClassRef mn (cstr ^. #classRef))
   in tc ty

mkDeriveRule :: PC.ModuleName -> PC.TyDefs -> PC.ClassRels -> PC.Derive -> Either P.CompilerError Clause
mkDeriveRule mn tyds clrels drv =
  let ty = E.fromTy $ drv ^. #constraint . #argument
      tc = tclass (PC.qualifyClassRef mn $ drv ^. #constraint . #classRef)
   in do
        supsBody <- mkSupersBody mn clrels (drv ^. #constraint)
        case E.runEval' mn tyds ty of
          Left err -> Left err
          Right ty' -> Right $ tc (tyToTerm mn ty) @<= (tc (tyToTerm mn ty') : supsBody)

mkInstanceRule :: PC.ModuleName -> PC.ClassRels -> PC.InstanceClause -> Either P.CompilerError Clause
mkInstanceRule mn clrels inst =
  let thead = tyToTerm mn . E.fromTy $ inst ^. #head . #argument
      tc = tclass (PC.qualifyClassRef mn $ inst ^. #head . #classRef)
      body = termFromConstraint mn <$> inst ^. #constraints
   in do
        supsBody <- mkSupersBody mn clrels (inst ^. #head)
        return $ tc thead @<= (body ++ supsBody)

mkQuery :: Term -> Term
mkQuery (ML.Var vn) = tvar (ML.Atom . AText $ vn)
mkQuery (ML.Struct f args) = ML.Struct f (mkQuery <$> args)
mkQuery t = t

tyToTerm :: PC.ModuleName -> E.Ty -> Term
tyToTerm mn (E.TyRef tr) = ML.Atom . ATyName . PC.qualifyTyRef mn $ tr
tyToTerm _ (E.TyVar tv) = ML.Var (tv ^. #varName . #name)
tyToTerm _ (E.TyOpaque _) = topaque
tyToTerm mn (E.TyAbs args body _) = tabs (foldr (\(vn, _) t -> (ML.Atom . AVarName $ vn) @| t) nilt (OMap.assocs args)) (tyToTerm mn body)
tyToTerm mn (E.TyApp tf args _) = tapp (tyToTerm mn tf) (foldr (\ty t -> tyToTerm mn ty @| t) nilt args)
tyToTerm mn (E.TySum ctors _) = tsum $ foldr (\(cn, cp) t -> tctor (ML.Atom . AConstrName $ cn) (tyToTerm mn cp) @| t) nilt (OMap.assocs ctors)
tyToTerm mn (E.TyTuple fields _) = ttuple $ foldr (\fty t -> tyToTerm mn fty @| t) nilt fields
tyToTerm mn (E.TyRecord fields _) = trec $ foldr (\(fn, fty) t -> tfield (ML.Atom . AFieldName $ fn) (tyToTerm mn fty) @| t) nilt (OMap.assocs fields)

-- | Solver.
runSolve :: PC.ModuleName -> [Clause] -> [Term] -> Either P.CompilerError ()
runSolve _mn clauses goals = do
  let (errOrRes, _log) = ML.solve clauses goals
  case errOrRes of
    Left mlErr -> Left $ fromMiniLogError mlErr
    Right _ -> return ()

fromMiniLogError :: ML.MiniLogError Funct Atom -> P.CompilerError
fromMiniLogError (ML.CycledGoalsError gs) =
  defMessage
    & P.internalErrors
      .~ [ defMessage & P.msg .~ Text.pack ("TODO: Cycled goals detected " <> show gs)
         ]
fromMiniLogError (ML.OverlappingClausesError clauses) =
  defMessage
    & P.internalErrors
      .~ [ defMessage & P.msg .~ Text.pack ("TODO: Overlapping clauses " <> show clauses)
         ]
fromMiniLogError (ML.MissingClauseError g) =
  defMessage
    & P.internalErrors
      .~ [ defMessage & P.msg .~ Text.pack ("TODO: MissingClause to solve " <> show g)
         ]
fromMiniLogError (ML.InternalError ierr) =
  defMessage
    & P.internalErrors
      .~ [ defMessage & P.msg .~ Text.pack ("TODO: MiniLog reported an internal error " <> show ierr)
         ]
