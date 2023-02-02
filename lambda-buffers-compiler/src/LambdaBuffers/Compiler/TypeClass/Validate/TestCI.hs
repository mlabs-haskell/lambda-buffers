-- TODO(gnumonik): Move into a real test suite

module LambdaBuffers.Compiler.TypeClass.Validate.TestCI (testCompilerInput) where

import Data.List.NonEmpty qualified as NE
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as P (
  ClassDef (
    ClassDef,
    classArgs,
    className,
    documentation,
    sourceInfo,
    supers
  ),
  ClassName (ClassName),
  CompilerInput (CompilerInput),
  ConstrName (ConstrName),
  Constraint (Constraint, argument, classRef, sourceInfo),
  Constructor (Constructor),
  ForeignRef (ForeignRef),
  InstanceClause (..),
  Kind (Kind),
  KindRefType (KType),
  KindType (KindRef),
  LocalRef (LocalRef),
  Module (..),
  ModuleName (ModuleName),
  ModuleNamePart (ModuleNamePart),
  Product (TupleI),
  SourceInfo (SourceInfo),
  SourcePosition (SourcePosition),
  Sum (Sum, constructors, sourceInfo),
  Tuple (Tuple),
  Ty (TyAppI, TyRefI, TyVarI),
  TyAbs (TyAbs, sourceInfo, tyArgs, tyBody),
  TyApp (TyApp, sourceInfo, tyArgs, tyFunc),
  TyArg (TyArg),
  TyBody (OpaqueI, SumI),
  TyDef (TyDef, sourceInfo, tyAbs, tyName),
  TyName (TyName),
  TyRef (ForeignI, LocalI),
  TyVar (TyVar),
  VarName (VarName),
 )

testCompilerInput :: P.CompilerInput
testCompilerInput = P.CompilerInput [prelude, foo]

si :: P.SourceInfo
si = P.SourceInfo "" (P.SourcePosition 0 0) (P.SourcePosition 0 0)

foo :: P.Module
foo =
  P.Module
    { moduleName = P.ModuleName [P.ModuleNamePart "Foo" si] si
    , typeDefs = [fooDef]
    , classDefs = []
    , instances = [cFoo]
    , imports = [P.ModuleName [P.ModuleNamePart "Prelude" si] si]
    , sourceInfo = si
    }
  where
    fooDef =
      P.TyDef
        { tyName = P.TyName "Foo" si
        , tyAbs =
            P.TyAbs
              { tyArgs = []
              , tyBody =
                  P.SumI $
                    P.Sum
                      { constructors =
                          NE.singleton $
                            P.Constructor
                              (P.ConstrName "Foo" si)
                              ( P.TupleI
                                  ( P.Tuple
                                      [ P.TyAppI $
                                          P.TyApp
                                            { tyFunc = P.TyRefI $ P.ForeignI $ P.ForeignRef (P.TyName "Maybe" si) (P.ModuleName [P.ModuleNamePart "Prelude" si] si) si
                                            , tyArgs =
                                                NE.fromList
                                                  [ P.TyRefI $ P.ForeignI $ P.ForeignRef (P.TyName "Int" si) (P.ModuleName [P.ModuleNamePart "Prelude" si] si) si
                                                  ]
                                            , sourceInfo = si
                                            }
                                      ]
                                      si
                                  )
                              )
                      , sourceInfo = si
                      }
              , sourceInfo = si
              }
        , sourceInfo = si
        }

    cFoo =
      P.InstanceClause
        { classRef = P.ForeignI $ P.ForeignRef (P.TyName "C" si) (P.ModuleName [P.ModuleNamePart "Prelude" si] si) si
        , head = P.TyRefI $ P.LocalI (P.LocalRef (P.TyName "Foo" si) si)
        , constraints = []
        , sourceInfo = si
        }

foo2 :: P.Module
foo2 =
  P.Module
    { moduleName = P.ModuleName [P.ModuleNamePart "Foo2" si] si
    , typeDefs = [fooDef]
    , classDefs = []
    , instances = [cFoo]
    , imports = [] -- [P.ModuleName [P.ModuleNamePart "Prelude" si] si]
    , sourceInfo = si
    }
  where
    fooDef =
      P.TyDef
        { tyName = P.TyName "Foo" si
        , tyAbs =
            P.TyAbs
              { tyArgs = []
              , tyBody =
                  P.SumI $
                    P.Sum
                      { constructors =
                          NE.singleton $
                            P.Constructor
                              (P.ConstrName "Foo" si)
                              ( P.TupleI
                                  ( P.Tuple
                                      [ P.TyAppI $
                                          P.TyApp
                                            { tyFunc = P.TyRefI $ P.ForeignI $ P.ForeignRef (P.TyName "Maybe" si) (P.ModuleName [P.ModuleNamePart "Prelude" si] si) si
                                            , tyArgs =
                                                NE.fromList
                                                  [ P.TyRefI $ P.ForeignI $ P.ForeignRef (P.TyName "Int" si) (P.ModuleName [P.ModuleNamePart "Prelude" si] si) si
                                                  ]
                                            , sourceInfo = si
                                            }
                                      ]
                                      si
                                  )
                              )
                      , sourceInfo = si
                      }
              , sourceInfo = si
              }
        , sourceInfo = si
        }

    cFoo =
      P.InstanceClause
        { classRef = P.ForeignI $ P.ForeignRef (P.TyName "C" si) (P.ModuleName [P.ModuleNamePart "Prelude" si] si) si
        , head = P.TyRefI $ P.LocalI (P.LocalRef (P.TyName "Foo" si) si)
        , constraints = []
        , sourceInfo = si
        }

prelude :: P.Module
prelude =
  P.Module
    { moduleName = mname
    , typeDefs = [intDef, maybeDef]
    , classDefs = [cDef]
    , instances = [cInt, cMaybe]
    , imports = []
    , sourceInfo = si
    }
  where
    mname = P.ModuleName [P.ModuleNamePart "Prelude" si] si

    intDef =
      P.TyDef
        { tyName = P.TyName "Int" si
        , tyAbs =
            P.TyAbs
              { tyArgs = []
              , tyBody = P.OpaqueI si
              , sourceInfo = si
              }
        , sourceInfo = si
        }

    maybeDef =
      P.TyDef
        { tyName = P.TyName "Maybe" si
        , tyAbs =
            P.TyAbs
              { tyArgs = [P.TyArg (P.VarName "a" si) (P.Kind (P.KindRef P.KType) si) si]
              , tyBody =
                  P.SumI $
                    P.Sum
                      { constructors =
                          NE.fromList
                            [ P.Constructor (P.ConstrName "Nothing" si) (P.TupleI (P.Tuple [] si))
                            , P.Constructor (P.ConstrName "Just" si) (P.TupleI (P.Tuple [P.TyVarI $ P.TyVar (P.VarName "a" si) si] si))
                            ]
                      , sourceInfo = si
                      }
              , sourceInfo = si
              }
        , sourceInfo = si
        }

    cDef =
      P.ClassDef
        { className = P.ClassName "C" si
        , classArgs = P.TyArg (P.VarName "a" si) (P.Kind (P.KindRef P.KType) si) si
        , supers = []
        , documentation = ""
        , sourceInfo = si
        }

    cInt :: P.InstanceClause
    cInt =
      P.InstanceClause
        { classRef = P.LocalI (P.LocalRef (P.TyName "C" si) si)
        , head = P.TyRefI $ P.LocalI (P.LocalRef (P.TyName "Int" si) si)
        , constraints = []
        , sourceInfo = si
        }

    cMaybe :: P.InstanceClause
    cMaybe =
      P.InstanceClause
        { classRef = P.LocalI (P.LocalRef (P.TyName "C" si) si)
        , head =
            P.TyAppI $
              P.TyApp
                { tyFunc = P.TyRefI $ P.LocalI (P.LocalRef (P.TyName "Maybe" si) si)
                , tyArgs =
                    NE.fromList
                      [ P.TyVarI (P.TyVar (P.VarName "a" si) si)
                      ]
                , sourceInfo = si
                }
        , constraints =
            [ P.Constraint
                { classRef = P.LocalI (P.LocalRef (P.TyName "C" si) si)
                , argument = P.TyVarI (P.TyVar (P.VarName "a" si) si)
                , sourceInfo = si
                }
            ]
        , sourceInfo = si
        }
