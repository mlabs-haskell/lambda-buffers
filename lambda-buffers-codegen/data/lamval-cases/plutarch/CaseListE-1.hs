import "plutarch" qualified Plutarch.Prelude (PCons, PNil, pcon)

pmatch xs (\x0 -> case x0 of
                            Plutarch.Prelude.PNil -> Plutarch.Prelude.pcon Plutarch.Prelude.PNil
                            Plutarch.Prelude.PCons x1 x2 -> pmatch x2 (\x3 -> case x3 of
                                                                                        Plutarch.Prelude.PNil -> xs
                                                                                        Plutarch.Prelude.PCons x4 x5 -> pmatch x5 (\x6 -> case x6 of
                                                                                                                                                    Plutarch.Prelude.PNil -> Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (x1) (Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (x4) (Plutarch.Prelude.pcon Plutarch.Prelude.PNil))))
                                                                                                                                                    Plutarch.Prelude.PCons x7 x8 -> pmatch x8 (\x9 -> case x9 of
                                                                                                                                                                                                                Plutarch.Prelude.PNil -> xs
                                                                                                                                                                                                                Plutarch.Prelude.PCons x10 x11 -> pmatch x11 (\x12 -> case x12 of
                                                                                                                                                                                                                                                                                 Plutarch.Prelude.PNil -> Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (x1) (Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (x4) (Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (x7) (Plutarch.Prelude.pcon (Plutarch.Prelude.PCons (x10) (Plutarch.Prelude.pcon Plutarch.Prelude.PNil))))))))
                                                                                                                                                                                                                                                                                 Plutarch.Prelude.PCons x13 x14 -> xs)))))
