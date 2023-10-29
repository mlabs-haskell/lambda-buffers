import "plutarch" qualified Plutarch.Prelude (pcon, pmatch)

Plutarch.Prelude.pmatch fooProduct (\(FooProduct x0 x1 x2) -> Plutarch.Prelude.pcon (FooProduct (x0) (x1) (x2)))
