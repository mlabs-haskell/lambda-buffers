import "plutarch" qualified Plutarch.Prelude (PCons, PNil, pcon, pconstant)

Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (Plutarch.Prelude.pconstant 1) (Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (Plutarch.Prelude.pconstant 2) (Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (a) (Plutarch.Prelude.pcon Plutarch.Prelude.PNil))))))
