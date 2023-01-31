module Test.Samples.Proto.Module where

import Control.Lens ((%~), (&), (.~))
import Data.List.NonEmpty (NonEmpty ((:|)), cons)
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.SourceInfo

modMaybe =
  P.Module
    { P.moduleName =
        P.ModuleName
          { P.parts = [P.ModuleNamePart "Module" esi]
          , P.sourceInfo = esi
          }
    , P.typeDefs =
        [ P.TyDef
            { P.tyName = P.TyName "Maybe" esi
            , P.tyAbs =
                P.TyAbs
                  { P.tyArgs =
                      [ P.TyArg
                          { P.argName = P.VarName "a" esi
                          , P.argKind =
                              P.Kind
                                { P.kind = P.KindRef P.KType
                                -- , P.sourceInfo = esi
                                }
                          , P.sourceInfo = esi
                          }
                      ]
                  , P.tyBody =
                      P.SumI $
                        P.Sum
                          { constructors =
                              P.Constructor
                                { P.constrName = P.ConstrName {P.name = "Nothing", P.sourceInfo = esi}
                                , P.product = P.TupleI $ P.Tuple {P.fields = [], P.sourceInfo = esi}
                                }
                                :| [ P.Constructor
                                      { P.constrName = P.ConstrName {P.name = "Just", P.sourceInfo = esi}
                                      , P.product =
                                          P.TupleI $
                                            P.Tuple
                                              { P.fields =
                                                  [ P.TyVarI
                                                      ( P.TyVar
                                                          { P.varName =
                                                              P.VarName
                                                                { P.name = "a"
                                                                , P.sourceInfo = esi
                                                                }
                                                          , P.sourceInfo = esi
                                                          }
                                                      )
                                                  ]
                                              , P.sourceInfo = esi
                                              }
                                      }
                                   ]
                          , sourceInfo = esi
                          }
                  , P.sourceInfo = esi
                  }
            , P.sourceInfo = esi
            }
        ]
    , P.classDefs = mempty
    , P.instances = mempty
    , P.sourceInfo = esi
    }

{- | 1 Module containing
  Maybe = ...

  and adding:
  B a = B Maybe

 Should fail as B a defaults to B :: Type -> Type and Maybe is inferred as
 Type -> Type. This is an inconsistency failure.
-}
addMod :: P.Module
addMod =
  modMaybe
    & #typeDefs
      %~ ( <>
            [ -- B a = B Maybe
              P.TyDef
                { P.tyName = P.TyName "B" esi
                , P.tyAbs =
                    P.TyAbs
                      { P.tyArgs =
                          [ P.TyArg
                              { P.argName = P.VarName "a" esi
                              , P.argKind =
                                  P.Kind
                                    { P.kind = P.KindRef P.KType
                                    -- , P.sourceInfo = esi
                                    }
                              , P.sourceInfo = esi
                              }
                          ]
                      , P.tyBody =
                          P.SumI $
                            P.Sum
                              { constructors =
                                  P.Constructor
                                    { P.constrName = P.ConstrName {P.name = "B", P.sourceInfo = esi}
                                    , P.product =
                                        P.TupleI $
                                          P.Tuple
                                            { P.fields =
                                                [ P.TyRefI $
                                                    P.LocalI $
                                                      P.LocalRef
                                                        { P.tyName = P.TyName {P.name = "Maybe", P.sourceInfo = esi}
                                                        , P.sourceInfo = esi
                                                        }
                                                ]
                                            , P.sourceInfo = esi
                                            }
                                    }
                                    :| []
                              , sourceInfo = esi
                              }
                      , P.sourceInfo = esi
                      }
                , P.sourceInfo = esi
                }
            ]
         )
