import "lbr-plutarch" qualified LambdaBuffers.Runtime.Plutarch (pcon)
import "plutarch" qualified Plutarch.Prelude (PCons, PNil, pconstant)

LambdaBuffers.Runtime.Plutarch.pcon (Plutarch.Prelude.PCons (Plutarch.Prelude.pconstant 1) (LambdaBuffers.Runtime.Plutarch.pcon (Plutarch.Prelude.PCons (Plutarch.Prelude.pconstant 2) (LambdaBuffers.Runtime.Plutarch.pcon (Plutarch.Prelude.PCons (a) (LambdaBuffers.Runtime.Plutarch.pcon Plutarch.Prelude.PNil))))))
