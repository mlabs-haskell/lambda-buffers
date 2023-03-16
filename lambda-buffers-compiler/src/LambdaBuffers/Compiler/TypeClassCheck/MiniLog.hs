{-# LANGUAGE PatternSynonyms #-}

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

import Control.Lens (view, (^.))
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Maybe (maybeToList)
import Data.Text (Text)
import LambdaBuffers.Compiler.MiniLog ((@), (@<=))
import LambdaBuffers.Compiler.MiniLog qualified as ML
import LambdaBuffers.Compiler.MiniLog.UniFdSolver qualified as ML
import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Eval qualified as E
import LambdaBuffers.Compiler.ProtoCompat.Indexing qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Utils qualified as PC
import LambdaBuffers.Compiler.TypeClassCheck.Errors (cycledGoalsError, deriveOpaqueError, internalError, internalError', mappendErrs, memptyErr, missingRuleError, overlappingRulesError, unboundTyClassRefError')
import Proto.Compiler qualified as P

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
  show (ATyName (mn, tn)) = PC.withInfoLess mn (show . PC.prettyModuleName) <> "." <> PC.withInfoLess tn (show . view #name)
  show ANil = "[]"
  show (AText txt) = show txt

data Funct
  = FSum
  | FTuple
  | FRec
  | FOpaque
  | FApp
  | FAbs
  | FClass PC.QClassName (Maybe (PC.ModuleName, PC.Constraint))
  | FField
  | FConstructor
  | FListCons
  | -- | Used when solving a term that contains a variable.
    FVar
  deriving stock (Ord)

{- | Used by MiniLog to perform unification.
 The only reason we're not deriving this is because we want to ignore some
 information we need passed around for proper error reporting (`FClass`).
-}
instance Eq Funct where
  (==) :: Funct -> Funct -> Bool
  FSum == FSum = True
  FTuple == FTuple = True
  FRec == FRec = True
  FOpaque == FOpaque = True
  FApp == FApp = True
  FAbs == FAbs = True
  FField == FField = True
  FConstructor == FConstructor = True
  FListCons == FListCons = True
  FVar == FVar = True
  (FClass qcnL _) == (FClass qcnR _) = qcnL == qcnR
  _ == _ = False

-- | Used by MiniLog to print as Prolog functors.
instance Show Funct where
  show (FClass (mn, cn) _) = PC.withInfoLess mn (show . PC.prettyModuleName) <> "." <> PC.withInfoLess cn (show . view #name)
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

-- | Pattern synonims and constructors.
pattern TyNameT :: PC.QTyName -> Term
pattern TyNameT qtyn <- ML.Atom (ATyName qtyn)
  where
    TyNameT qtyn = ML.Atom (ATyName qtyn)

pattern ConsT :: Term -> Term -> Term
pattern ConsT h t <- ML.Struct FListCons [h, t]
  where
    ConsT = (@|)

(@|) :: Term -> Term -> Term
(@|) h t = ML.struct FListCons [h, t]

pattern NilT :: Term
pattern NilT <- ML.Atom ANil
  where
    NilT = nilt

nilt :: Term
nilt = ML.Atom ANil

pattern AppT :: Term -> Term -> Term
pattern AppT f args <- ML.Struct FApp [f, args]
  where
    AppT = tapp

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

pattern QVarT :: Text -> Term
pattern QVarT vn <- ML.Struct FVar [ML.Atom (AText vn)]
  where
    QVarT vn = ML.Struct FVar [ML.Atom (AText vn)]

tvar :: Term -> Term
tvar vn = ML.struct FVar [vn]

pattern OpaqueT :: Term
pattern OpaqueT <- ML.Struct FOpaque []
  where
    OpaqueT = topaque

topaque :: Term
topaque = ML.struct FOpaque []

pattern ConstraintT :: PC.QClassName -> Term -> Term
pattern ConstraintT qcn ty <- ML.Struct (FClass qcn _) [ty]
  where
    ConstraintT = tconstraint

tconstraint :: PC.QClassName -> Term -> Term
tconstraint qcn ty = ML.struct (FClass qcn Nothing) [ty]

tconstraint' :: PC.ModuleName -> PC.Constraint -> Term -> Term
tconstraint' mn cstr a =
  let qcn = PC.qualifyClassRef mn (cstr ^. #classRef)
   in ML.struct (FClass qcn (Just (mn, cstr))) [a]

-- | LambdaBuffers type class rules.
mkSumRule :: PC.QClassName -> Clause
mkSumRule qcn =
  let tc = tconstraint qcn
   in tc (tsum (tctor ("C" @) ("P" @) @| ("T" @)))
        @<= [ tc ("P" @)
            , tc $ tsum ("T" @)
            ]

mkTupleRule :: PC.QClassName -> Clause
mkTupleRule qcn =
  let tc = tconstraint qcn
   in tc (ttuple (("H" @) @| ("T" @)))
        @<= [ tc ("H" @)
            , tc $ ttuple ("T" @)
            ]

mkRecRule :: PC.QClassName -> Clause
mkRecRule qcn =
  let tc = tconstraint qcn
   in tc (trec (tfield ("Fn" @) ("FTy" @) @| ("Fs" @)))
        @<= [ tc ("FTy" @)
            , tc $ trec ("Fs" @)
            ]

mkVoidRule :: PC.QClassName -> Clause
mkVoidRule qcn = let tc = tconstraint qcn in tc (tsum nilt) @<= []

mkUnitRuleTuple :: PC.QClassName -> Clause
mkUnitRuleTuple qcn = let tc = tconstraint qcn in tc (ttuple nilt) @<= []

mkUnitRuleRec :: PC.QClassName -> Clause
mkUnitRuleRec qcn = let tc = tconstraint qcn in tc (trec nilt) @<= []

mkVarRule :: PC.QClassName -> Clause
mkVarRule qcn = let tc = tconstraint qcn in tc (tvar ("X" @)) @<= []

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
        Nothing -> Left $ unboundTyClassRefError' mn (cstr ^. #classRef)
        Just sups -> Right ((`tconstraint` ty) <$> sups)

mkDeriveRule :: PC.ModuleName -> PC.TyDefs -> PC.ClassRels -> PC.Derive -> Either P.CompilerError Clause
mkDeriveRule mn tyds clrels drv =
  let ty = E.fromTy $ drv ^. #constraint . #argument
      hd = termFromConstraint mn (drv ^. #constraint)
      tc = tcstrFromClassRef mn (drv ^. #constraint . #classRef)
   in do
        supsBody <- mkSupersBody mn clrels (drv ^. #constraint)
        case E.runEval' mn tyds ty of
          Left err -> Left err
          Right ty' -> Right $ hd @<= (tc (tyToTerm mn ty') : supsBody)

mkInstanceRule :: PC.ModuleName -> PC.ClassRels -> PC.InstanceClause -> Either P.CompilerError Clause
mkInstanceRule mn clrels inst =
  let hd = termFromConstraint mn (inst ^. #head)
      body = termFromConstraint mn <$> inst ^. #constraints
   in do
        supsBody <- mkSupersBody mn clrels (inst ^. #head)
        return $ hd @<= (body ++ supsBody)

mkQuery :: Term -> Term
mkQuery (ML.Var vn) = tvar (ML.Atom . AText $ vn)
mkQuery (ML.Struct f args) = ML.Struct f (mkQuery <$> args)
mkQuery t = t

termFromConstraint :: PC.ModuleName -> PC.Constraint -> Term
termFromConstraint mn cstr =
  let ty = tyToTerm mn . E.fromTy $ cstr ^. #argument
      tc = tconstraint' mn cstr
   in tc ty

tcstrFromClassRef :: PC.ModuleName -> PC.TyClassRef -> (Term -> Term)
tcstrFromClassRef mn cr = let qcn = PC.qualifyClassRef mn cr in tconstraint qcn

-- | Turn a canonical `E.Ty` representation into a `Term`.
tyToTerm :: PC.ModuleName -> E.Ty -> Term
tyToTerm mn (E.TyRef tr) = ML.Atom $ ATyName (PC.qualifyTyRef mn tr)
tyToTerm _ (E.TyVar tv) = ML.Var (tv ^. #varName . #name)
tyToTerm _ (E.TyOpaque _) = topaque
tyToTerm mn (E.TyAbs args body _) = tabs (foldr (\(vn, _) t -> (ML.Atom . AVarName $ vn) @| t) nilt (OMap.assocs args)) (tyToTerm mn body)
tyToTerm mn (E.TyApp tf args _) = tapp (tyToTerm mn tf) (foldr (\ty t -> tyToTerm mn ty @| t) nilt args)
tyToTerm mn (E.TySum ctors _) = tsum $ foldr (\(cn, cp) t -> tctor (ML.Atom . AConstrName $ cn) (tyToTerm mn cp) @| t) nilt (OMap.assocs ctors)
tyToTerm mn (E.TyProduct fields _) = ttuple $ foldr (\fty t -> tyToTerm mn fty @| t) nilt fields
tyToTerm mn (E.TyRecord fields _) = trec $ foldr (\(fn, fty) t -> tfield (ML.Atom . AFieldName $ fn) (tyToTerm mn fty) @| t) nilt (OMap.assocs fields)

{- | Solve/evaluate terms (goals) given some knowledge base (clauses).
 Tries each goal individually and collects all the errors.
-}
runSolve :: PC.ModuleName -> [Clause] -> [Term] -> Either P.CompilerError ()
runSolve mn clauses goals =
  let allErrs =
        foldr
          ( \goal errs -> case runSolve' mn clauses goal of
              Left err -> err `mappendErrs` errs
              Right _ -> errs
          )
          memptyErr
          goals
   in if allErrs == memptyErr then Right () else Left allErrs

-- | Tries to solve a single goal.
runSolve' :: PC.ModuleName -> [Clause] -> Term -> Either P.CompilerError ()
runSolve' locMn clauses goal = do
  let (errOrRes, mlTrace) = ML.solve clauses [goal]
  case errOrRes of
    Left mlErr -> do
      case goalToConstraint locMn goal of
        Nothing -> Left $ internalError' locMn ("Failed translating the current goal into a Proto.Compiler.Constraint\n" <> show goal)
        Just cstr -> Left $ fromMiniLogError locMn cstr mlTrace mlErr
    Right _ -> return ()

-- | Convert to API errors.
fromMiniLogError :: PC.ModuleName -> PC.Constraint -> [ML.MiniLogTrace Funct Atom] -> ML.MiniLogError Funct Atom -> P.CompilerError
fromMiniLogError locMn currentCstr _trace (ML.CycledGoalsError gs) =
  let userDefinedGoals = [c | g <- gs, c <- maybeToList (goalToConstraint locMn g)]
   in cycledGoalsError locMn currentCstr userDefinedGoals
fromMiniLogError locMn currentCstr _trace err@(ML.OverlappingClausesError clauses goal) =
  case originalConstraint `traverse` clauses of
    Nothing -> internalError locMn currentCstr ("Failed extracting the original `Constraint` when constructing a report for the `OverlappingRulesError`" <> show err)
    Just overlaps -> case goalToConstraint locMn goal of
      Nothing -> internalError locMn currentCstr ("Failed translating to `Constraint` when constructing a report for the `OverlappingRulesError`" <> show err)
      Just subCstr -> overlappingRulesError locMn currentCstr subCstr overlaps
fromMiniLogError locMn currentCstr trace err@(ML.MissingClauseError (ConstraintT _ OpaqueT)) =
  deriveOpaqueError' locMn currentCstr trace err
fromMiniLogError locMn currentCstr trace err@(ML.MissingClauseError (ConstraintT _ (AppT OpaqueT _args))) =
  deriveOpaqueError' locMn currentCstr trace err
fromMiniLogError locMn currentCstr _trace err@(ML.MissingClauseError forGoal) =
  case goalToConstraint locMn forGoal of
    Nothing -> internalError locMn currentCstr ("Failed translating to `Constraint` when constructing a report for the `MissingInstanceError`" <> show err)
    Just forCstr -> missingRuleError locMn currentCstr forCstr
fromMiniLogError locMn currentCstr _trace (ML.InternalError ierr) = internalError locMn currentCstr (show ierr)

{- | Searches through the `ML.MiniLogTrace` finding the `Clause` within which an errouneous call occurred.
 WARN(bladyjoker): This is brittle, consider adding the clause head as part of the `ML.MiniLogError`.
-}
deriveOpaqueError' :: forall {a}. Show a => PC.ModuleName -> PC.Constraint -> [ML.MiniLogTrace Funct Atom] -> a -> P.CompilerError
deriveOpaqueError' locMn currentCstr trace err = case reverse $ filter (\case ML.CallClause _ _ -> True; _ -> False) trace of
  (ML.CallClause cl _ : _) -> case originalConstraint cl of
    Nothing -> internalError locMn currentCstr ("Failed extracting the original constraint when constructing a report for the `DeriveOpaqueError`\n" <> show err)
    Just (_, forCstr) -> deriveOpaqueError locMn currentCstr forCstr
  _ -> internalError locMn currentCstr (show err)

{- | Turn a `Term` into a `PC.Ty`.
 This is only used for reporting error and thus only necessary translations are
 supplied. The `Term`s accepted here are the fully ground ones, meaning there's
 no `ML.Var` in them, only `QVar`s.
-}
termToTy :: PC.ModuleName -> Term -> Maybe PC.Ty
termToTy _locMn (QVarT vn) = Just . PC.TyVarI $ PC.TyVar (PC.VarName vn def)
termToTy locMn (TyNameT qtyn) = Just (PC.TyRefI $ PC.tyRefFromQualified locMn qtyn)
termToTy locMn (AppT f args) = PC.TyAppI <$> (PC.TyApp <$> termToTy locMn f <*> listToTys locMn args <*> pure def)
  where
    listToTys lmn (ConsT h t) = (:) <$> termToTy lmn h <*> listToTys lmn t
    listToTys _ NilT = Just []
    listToTys _ _ = Nothing
termToTy _ _ = Nothing

-- | Convert to a `PC.Constraint` (SourceInfo is meaningless here).
goalToConstraint :: PC.ModuleName -> Term -> Maybe PC.Constraint
goalToConstraint locMn (ConstraintT qcn arg) = PC.Constraint (PC.classRefFromQualified locMn qcn) <$> termToTy locMn arg <*> pure def
goalToConstraint _ _ = Nothing

-- | Clause heads have attached the original `PC.Constraint` and the `PC.ModuleName` they are defined in.
originalConstraint :: Clause -> Maybe (PC.ModuleName, PC.Constraint)
originalConstraint (ML.MkClause (ML.Struct (FClass _ origCstr) _) _) = origCstr
originalConstraint _ = Nothing
