import "plutarch" qualified Plutarch.Prelude (pcon, pmatch)

Plutarch.Prelude.pmatch fooSum (\x4 -> case x4 of
                                         FooSum'Bar x0 -> Plutarch.Prelude.pcon (FooSum'Bar (x0))
                                         FooSum'Baz x1 x2 x3 -> Plutarch.Prelude.pcon (FooSum'Baz (x1) (x2) (x3)))
