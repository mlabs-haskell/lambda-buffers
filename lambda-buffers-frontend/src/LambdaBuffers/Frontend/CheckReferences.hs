module LambdaBuffers.Frontend.CheckReferences (checkReferences) where

import Control.Monad.Trans.Reader (asks)
import Data.Foldable (for_)
import Data.Map qualified as Map
import LambdaBuffers.Frontend.Errors qualified as Errors
import LambdaBuffers.Frontend.Monad (FrontRead (current), FrontendT, throwE')
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (ClassConstraint (ClassConstraint), ClassDef (classSupers), ClassRef (ClassRef), Constraint (Constraint), Constructor (Constructor), Derive (Derive), Field (Field), InstanceClause (instanceBody, instanceHead), Module (moduleStatements), Product (Product), Record (Record), SourceInfo, Statement (StClassDef, StDerive, StInstanceClause, StTyDef), Sum (Sum), Ty (TyApp, TyRef', TyVar), TyBody (Opaque, ProductBody, RecordBody, SumBody), TyDef (tyBody), TyRef (TyRef))
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope, strip)

checkReferences :: Module SourceInfo -> (TyScope, ClassScope) -> FrontendT m ()
checkReferences m scope@(tyScope, classScope) = do
  for_
    (moduleStatements m)
    ( \case
        StTyDef tds -> checkTyDef tyScope tds
        StClassDef cds -> checkClassDef classScope cds
        StInstanceClause ics -> checkInstanceClause scope ics
        StDerive drv -> checkDerive scope drv
    )

checkTyDef :: TyScope -> TyDef SourceInfo -> FrontendT m ()
checkTyDef tyScope = checkBody . tyBody
  where
    checkBody :: TyBody SourceInfo -> FrontendT m ()
    checkBody (SumBody (Sum cs _)) = for_ cs checkConstructor
    checkBody (ProductBody (Product fields _)) = for_ fields (checkTy tyScope)
    checkBody (RecordBody (Record fields _)) = for_ fields (\(Field _ ty _) -> checkTy tyScope ty)
    checkBody Opaque = return ()

    checkConstructor :: Constructor SourceInfo -> FrontendT m ()
    checkConstructor (Constructor _ (Product tys _) _) = for_ tys (checkTy tyScope)

checkClassDef :: ClassScope -> ClassDef SourceInfo -> FrontendT m ()
checkClassDef classScope = checkSups . classSupers
  where
    checkSups :: [ClassConstraint SourceInfo] -> FrontendT m ()
    checkSups sups = for_ sups checkClassConstraint

    checkClassConstraint :: ClassConstraint SourceInfo -> FrontendT m ()
    checkClassConstraint (ClassConstraint cr _) = checkClassRef classScope cr

checkClassRef :: ClassScope -> ClassRef SourceInfo -> FrontendT m ()
checkClassRef classScope cr@(ClassRef mayAlias clN _) =
  if Map.member (strip <$> mayAlias, strip clN) classScope
    then return ()
    else do
      cm <- asks current
      throwE' $ Errors.classRefNotFoundErr cm cr classScope

checkInstanceClause :: (TyScope, ClassScope) -> InstanceClause SourceInfo -> FrontendT m ()
checkInstanceClause scope inst = do
  checkConstraint scope (instanceHead inst)
  for_ (instanceBody inst) (checkConstraint scope)

checkDerive :: (TyScope, ClassScope) -> Derive SourceInfo -> FrontendT m ()
checkDerive scope (Derive cstr) = checkConstraint scope cstr

checkConstraint :: (TyScope, ClassScope) -> Constraint SourceInfo -> FrontendT m ()
checkConstraint (tyScope, classScope) (Constraint cr tys _) = do
  checkClassRef classScope cr
  for_ tys (checkTy tyScope)

checkTy :: TyScope -> Ty SourceInfo -> FrontendT m ()
checkTy tyScope (TyApp tyF tyAs _) = checkTy tyScope tyF >> for_ tyAs (checkTy tyScope)
checkTy _tyScope (TyVar _) = return ()
checkTy tyScope (TyRef' tr@(TyRef mayAlias tyN _) _) =
  if Map.member (strip <$> mayAlias, strip tyN) tyScope
    then return ()
    else do
      cm <- asks current
      throwE' $ Errors.tyRefNotFoundErr cm tr tyScope
