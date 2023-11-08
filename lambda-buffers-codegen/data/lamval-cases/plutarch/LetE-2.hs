import "lbr-plutarch" qualified LambdaBuffers.Runtime.Plutarch (pcon)
import "plutarch" qualified Plutarch.Prelude (pmatch)

Plutarch.Prelude.pmatch fooProduct (\(FooProduct x0 x1 x2) -> LambdaBuffers.Runtime.Plutarch.pcon (FooProduct (x0) (x1) (x2)))
