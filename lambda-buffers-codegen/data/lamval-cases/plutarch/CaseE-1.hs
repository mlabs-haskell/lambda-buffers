import "lbr-plutarch" qualified LambdaBuffers.Runtime.Plutarch (pcon)
import "plutarch" qualified Plutarch.Prelude (pmatch)

Plutarch.Prelude.pmatch fooSum (\x4 -> case x4 of
                                         FooSum'Bar x0 -> LambdaBuffers.Runtime.Plutarch.pcon (FooSum'Bar (x0))
                                         FooSum'Baz x1 x2 x3 -> LambdaBuffers.Runtime.Plutarch.pcon (FooSum'Baz (x1) (x2) (x3)))
