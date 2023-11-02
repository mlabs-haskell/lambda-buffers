import "lbr-plutarch" qualified LambdaBuffers.Runtime.Plutarch (pcon)
import "plutarch" qualified Plutarch.Prelude (pconstant)

LambdaBuffers.Runtime.Plutarch.pcon (FooProduct (x) (Plutarch.Prelude.pconstant 1) (LambdaBuffers.Runtime.Plutarch.pcon (UnitProduct (Plutarch.Prelude.pconstant "works"))))
