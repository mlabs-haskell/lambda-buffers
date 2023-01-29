module LambdaBuffers.Compiler.Canonical () where

import Data.List.NonEmpty (nonEmpty)
import LambdaBuffers.Compiler.ProtoCompat.Types (Constructor (Constructor), Field (Field), KindRefType (KType), KindType (KindRef), Product (RecordI, TupleI), Record (Record), SourceInfo (SourceInfo), SourcePosition (SourcePosition), Sum (Sum), Tuple (Tuple), Ty (TyAppI, TyRefI, TyVarI), TyAbs (TyAbs), TyApp (TyApp), TyArg, TyBody (OpaqueI, SumI), TyRef, TyVar)

type CTyVar = TyVar
type CTyArg = TyArg
type CTyRef = TyRef
type COpaque = KindType

data CTy
  = CTyVar CTyVar
  | CTyApp CTy CTy
  | CTyAbs CTyArg CTy
  | CTyRef CTyRef
  | CEither CTy CTy
  | CProd CTy CTy
  | CUnit
  | CVoid
  | COpaque COpaque
  deriving stock (Show, Eq)

class Canonical a where
  toCanonical :: a -> CTy
  fromCanonical :: CTy -> Maybe a

instance Canonical Ty where
  toCanonical (TyVarI tv) = CTyVar tv
  toCanonical (TyAppI (TyApp tyF tyArgs _)) = let cTyF = toCanonical tyF in foldl (\t x -> CTyApp t (toCanonical x)) cTyF tyArgs
  toCanonical (TyRefI r) = CTyRef r

  fromCanonical :: CTy -> Maybe Ty
  fromCanonical (CTyVar vn) = Just $ TyVarI vn
  fromCanonical (CTyRef _) = Just $ TyRefI undefined
  fromCanonical (CTyAbs _ _) = Nothing
  fromCanonical (COpaque _) = Nothing
  fromCanonical CUnit = Nothing
  fromCanonical CVoid = Nothing
  fromCanonical (CEither _ _) = Nothing
  fromCanonical (CProd _ _) = Nothing
  fromCanonical (CTyApp cTyF cTyA) = do
    tyF <- fromCanonical cTyF
    tyAs <- folded [] cTyA
    tyAs' <- nonEmpty tyAs
    Just $ TyAppI (TyApp tyF tyAs' si)

folded :: Canonical a => [a] -> CTy -> Maybe [a]
folded f cty@(CTyAbs _ _) = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@CUnit = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@(COpaque _) = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@CVoid = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@(CEither _ _) = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@(CProd _ _) = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@(CTyVar _) = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f cty@(CTyRef _) = fromCanonical cty >>= \ty -> return $ f ++ [ty]
folded f (CTyApp cTyF cTyA) = do
  tyF <- fromCanonical cTyF
  tyA <- fromCanonical cTyA
  return (f ++ [tyF, tyA])

instance Canonical TyAbs where
  toCanonical :: TyAbs -> CTy
  toCanonical (TyAbs tyAs tyB _) = foldl (flip CTyAbs) (toCanonical tyB) tyAs
  fromCanonical :: CTy -> Maybe TyAbs
  fromCanonical = _

instance Canonical TyBody where
  toCanonical :: TyBody -> CTy
  toCanonical (OpaqueI _) = COpaque $ KindRef KType
  toCanonical (SumI s) = toCanonical s
  fromCanonical :: CTy -> Maybe TyBody
  fromCanonical = _

instance Canonical Sum where
  toCanonical :: Sum -> CTy
  toCanonical (Sum cs _) = foldl (\t x -> CEither t (toCanonical x)) CVoid cs
  fromCanonical :: CTy -> Maybe Sum
  fromCanonical = _

instance Canonical Constructor where
  toCanonical :: Constructor -> CTy
  toCanonical (Constructor _ prd) = toCanonical prd
  fromCanonical :: CTy -> Maybe Constructor
  fromCanonical = _

instance Canonical Product where
  toCanonical :: Product -> CTy
  toCanonical (RecordI (Record fs _)) = foldl (\t x -> CProd t (toCanonical x)) CUnit ((\(Field _ ty) -> ty) <$> fs)
  toCanonical (TupleI (Tuple tys _)) = foldl (\t x -> CProd t (toCanonical x)) CUnit tys
  fromCanonical :: CTy -> Maybe Product
  fromCanonical = _

si = SourceInfo "" (SourcePosition 0 0) (SourcePosition 0 0)
