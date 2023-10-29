import "plutarch" qualified Plutarch.Prelude (pcon, pconstant)

Plutarch.Prelude.pcon (FooProduct (x) (Plutarch.Prelude.pconstant 1) (Plutarch.Prelude.pcon (UnitProduct (Plutarch.Prelude.pconstant "works"))))
