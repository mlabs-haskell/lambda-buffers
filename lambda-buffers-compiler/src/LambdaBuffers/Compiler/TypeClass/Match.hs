module LambdaBuffers.Compiler.TypeClass.Match (matches) where

-- can't import the patterns explicitly?
import LambdaBuffers.Compiler.TypeClass.Pat (
  Pat (AppP, DecP, ProdP, RecP, RefP, SumP, VarP, (:*), (:=)),
 )

{- This is used as a predicate to filter instances or Gens which are structurally compatible
   with the argument type.

   The first argument is the inner Pat from an instance head or Gen.

   The second argument is the Pat representation of a type that we want to derive an instance / generate code for.

   NOTE: Is not bidirectional! The first Pat has to be more general than the first
         (more specifically: The second Pat should be a substitution instance of the first)
-}
matches :: Pat -> Pat -> Bool
matches t1 t2 | t1 == t2 = True -- need the guard
matches (VarP _) _ = True
matches (x :* xs) (x' :* xs') = matches x x' && matches xs xs'
matches (l := t) (l' := t') = matches l l' && matches t t'
matches (ProdP xs) (ProdP xs') = matches xs xs'
matches (RecP xs) (RecP xs') = matches xs xs'
matches (SumP xs) (SumP xs') = matches xs xs'
matches (AppP t1 t2) (AppP t1' t2') = matches t1 t1' && matches t2 t2'
matches (RefP mn t1) (RefP mn' t2) = matches mn mn' && matches t1 t2
matches (DecP t1 t2 t3) (DecP t1' t2' t3') =
  matches t1 t1' && matches t2 t2' && matches t3 t3'
matches _ _ = False
