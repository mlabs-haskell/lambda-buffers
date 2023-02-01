module Test.Samples.Proto.TyDef (tyDef'maybe, tyDef'incoherent) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import LambdaBuffers.Compiler.ProtoCompat qualified as P
import Test.Samples.Proto.SourceInfo (sourceInfo'empty)

-- | Maybe tyDef.
tyDef'maybe :: P.TyDef
tyDef'maybe =
  P.TyDef
    { P.tyName = P.TyName "Maybe" sourceInfo'empty
    , P.tyAbs =
        P.TyAbs
          { P.tyArgs =
              [ P.TyArg
                  { P.argName = P.VarName "a" sourceInfo'empty
                  , P.argKind =
                      P.Kind
                        { P.kind = P.KindRef P.KType
                        }
                  , P.sourceInfo = sourceInfo'empty
                  }
              ]
          , P.tyBody =
              P.SumI $
                P.Sum
                  { constructors =
                      P.Constructor
                        { P.constrName = P.ConstrName {P.name = "Nothing", P.sourceInfo = sourceInfo'empty}
                        , P.product = P.TupleI $ P.Tuple {P.fields = [], P.sourceInfo = sourceInfo'empty}
                        }
                        :| [ P.Constructor
                              { P.constrName = P.ConstrName {P.name = "Just", P.sourceInfo = sourceInfo'empty}
                              , P.product =
                                  P.TupleI $
                                    P.Tuple
                                      { P.fields =
                                          [ P.TyVarI
                                              ( P.TyVar
                                                  { P.varName =
                                                      P.VarName
                                                        { P.name = "a"
                                                        , P.sourceInfo = sourceInfo'empty
                                                        }
                                                  , P.sourceInfo = sourceInfo'empty
                                                  }
                                              )
                                          ]
                                      , P.sourceInfo = sourceInfo'empty
                                      }
                              }
                           ]
                  , sourceInfo = sourceInfo'empty
                  }
          , P.sourceInfo = sourceInfo'empty
          }
    , P.sourceInfo = sourceInfo'empty
    }

-- | B a = B Maybe
tyDef'incoherent :: P.TyDef
tyDef'incoherent =
  P.TyDef
    { P.tyName = P.TyName "B" sourceInfo'empty
    , P.tyAbs =
        P.TyAbs
          { P.tyArgs =
              [ P.TyArg
                  { P.argName = P.VarName "a" sourceInfo'empty
                  , P.argKind =
                      P.Kind
                        { P.kind = P.KindRef P.KType
                        }
                  , P.sourceInfo = sourceInfo'empty
                  }
              ]
          , P.tyBody =
              P.SumI $
                P.Sum
                  { constructors =
                      P.Constructor
                        { P.constrName = P.ConstrName {P.name = "B", P.sourceInfo = sourceInfo'empty}
                        , P.product =
                            P.TupleI $
                              P.Tuple
                                { P.fields =
                                    [ P.TyRefI $
                                        P.LocalI $
                                          P.LocalRef
                                            { P.tyName = P.TyName {P.name = "Maybe", P.sourceInfo = sourceInfo'empty}
                                            , P.sourceInfo = sourceInfo'empty
                                            }
                                    ]
                                , P.sourceInfo = sourceInfo'empty
                                }
                        }
                        :| []
                  , sourceInfo = sourceInfo'empty
                  }
          , P.sourceInfo = sourceInfo'empty
          }
    , P.sourceInfo = sourceInfo'empty
    }
