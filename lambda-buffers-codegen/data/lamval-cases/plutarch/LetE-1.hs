import "lbr-plutarch" qualified LambdaBuffers.Runtime.Plutarch (pcon)
import "plutarch" qualified Plutarch.Prelude (pmatch)

Plutarch.Prelude.pmatch unitProduct (\(UnitProduct x0) -> LambdaBuffers.Runtime.Plutarch.pcon (UnitProduct (x0)))
