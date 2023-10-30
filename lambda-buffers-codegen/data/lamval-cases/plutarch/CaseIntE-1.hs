import "plutarch" qualified Plutarch.Prelude ((#==), pconstant, pif)

Plutarch.Prelude.pif ((Plutarch.Prelude.#==) (int) (Plutarch.Prelude.pconstant 1)) (Plutarch.Prelude.pconstant 1) (Plutarch.Prelude.pif ((Plutarch.Prelude.#==) (int) (Plutarch.Prelude.pconstant (-1))) (Plutarch.Prelude.pconstant (-1)) (int))
