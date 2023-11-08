import "lbr-plutarch" qualified LambdaBuffers.Runtime.Plutarch (pcon)
import "plutarch" qualified Plutarch.Prelude (pconstant)

LambdaBuffers.Runtime.Plutarch.pcon (FooSum'Bar (Plutarch.Prelude.pconstant "works"))
