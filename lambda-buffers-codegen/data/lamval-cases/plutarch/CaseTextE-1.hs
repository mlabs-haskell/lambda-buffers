import "plutarch" qualified Plutarch.Prelude ((#==), pconstant, pif)

Plutarch.Prelude.pif ((Plutarch.Prelude.#==) (txt) (Plutarch.Prelude.pconstant "a")) (Plutarch.Prelude.pconstant "a it is") (Plutarch.Prelude.pif ((Plutarch.Prelude.#==) (txt) (Plutarch.Prelude.pconstant "b")) (Plutarch.Prelude.pconstant "b it is") (txt))
