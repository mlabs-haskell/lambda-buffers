import "plutarch" qualified Plutarch.Prelude ((#==), pconstant, pif)

Plutarch.Prelude.pif (int (Plutarch.Prelude.#==) Plutarch.Prelude.pconstant 1) (Plutarch.Prelude.pconstant 1) (Plutarch.Prelude.pif (int (Plutarch.Prelude.#==) Plutarch.Prelude.pconstant (-1)) (Plutarch.Prelude.pconstant (-1)) (int))
