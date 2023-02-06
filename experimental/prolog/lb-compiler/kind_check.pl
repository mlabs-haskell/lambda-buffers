:-module(kind_check, [kind_check/2]).

:- set_prolog_flag(double_quotes, string).
:- use_module(compiler_pb).
:- use_module(aux).
:- use_module(proto_aux).

kind_check(CompIn, Sol) :-
    findall(M_,
            (
                member(M, CompIn.modules),
                findall(TyDef,
                        module_ty_def(M, TyDef),
                        TyDefs),
                M_ = M.put(_{type_defs: TyDefs})
            ),
            Modules),
    make_kind_check_ctx(Modules, Sol),
    member(Mod, Modules),
    kind_check_module(Sol, Mod).

make_kind_check_ctx(Modules, Sol) :-
    findall(R, (
                member(M, Modules),
                member(TyDef, M.type_defs),
                R = M.module_name-TyDef.ty_name-_K
            ),
            Sol).

module_ty_def(M, TyDef) :-
    member(TyDef, M.type_defs).

module_ty_def(M, TyDef) :-
    member(TyDef_, M.type_defs),
    (
        _{ty_body: TyBody} :< TyDef_;
        _{ty_abs: _{ty_body: TyBody}} :< TyDef_
    ),
    sum_ty_def(TyDef_.ty_name, TyBody, TyDef).

sum_ty_def(TyName, '.lambdabuffers.compiler.TyBody'{ sum: Sum }, TyDef) :-
    sum_ty(Sum, Ty), % NOTE(bladyjoker): Should fail in the future if Opaque
    sum_ty_name(TyName, SumTyName),
    (
        TyDef = '.lambdabuffers.compiler.TyDef'{
                    ty_name: SumTyName,
                    ty_abs: Ty,
                    source_info: _Todo
                };
        (
            member(Cons, Sum.constructors),
            prod_ty_def(TyName, Cons, TyDef)
        )
    ).

prod_ty_def(TyName, Cons, '.lambdabuffers.compiler.TyDef'{
                              ty_name: ProdTyName,
                              ty_abs: Ty,
                              source_info: _Todo
                          }) :-

    prod_ty(Cons.product, Ty),
    prod_ty_name(TyName, Cons.constr_name, ProdTyName).

sum_ty_name(TyName, '.lambdabuffers.compiler.TyName'{ name: SumTyName, source_info: TyName.source_info }) :-
    string_concat(TyName.name, "'Sum", SumTyName).

sum_ty('.lambdabuffers.compiler.Sum'{ constructors: [], source_info: Si}, '.lambdabuffers.compiler.TyBody'{ opaque: '.lambdabuffers.compiler.Opaque'{source_info: Si}}) :-
    print_message(info, reached_sum_void).

sum_ty('.lambdabuffers.compiler.Sum'{ constructors: [C|Cs], source_info: Si}, Ty) :-
    kind_ref(type, K),
    sum_ty('.lambdabuffers.compiler.Sum'{ constructors: Cs, source_info: Si}, TyBody),
    ty_abs(['.lambdabuffers.compiler.TyArg'{
               arg_kind: K,
               arg_name: '.lambdabuffers.compiler.VarName'{ name: C.constr_name.name, source_info: C.constr_name.source_info}
           }], TyBody, Ty).

prod_ty_name(TyName, ConstrName, '.lambdabuffers.compiler.TyName'{ name: ProdTyName, source_info: ConstrName.source_info }) :-
    string_concat(TyName.name, "'Sum'", SumTyName),
    string_concat(SumTyName, ConstrName.name, ProdTyName).

prod_ty('.lambdabuffers.compiler.Product'{ ntuple: _{fields: [], source_info: _}, source_info: _ }, TyAbs) :-
    ty_abs([], '.lambdabuffers.compiler.TyBody'{ opaque: '.lambdabuffers.compiler.Opaque'{ source_info: _}}, TyAbs),
    print_message(info, reached_tuple_unit).

prod_ty('.lambdabuffers.compiler.Product'{ ntuple: _{fields: [_|Fs], source_info: _}, source_info: _}, Ty) :-
    kind_ref(type, K),
    prod_ty('.lambdabuffers.compiler.Product'{
                ntuple: '.lambdabuffers.compiler.Product.NTuple'{ fields: Fs, source_info: _},
                source_info: _
            }, TyBody),
    ty_abs([
            '.lambdabuffers.compiler.TyArg'{
               arg_kind: K,
               arg_name: '.lambdabuffers.compiler.VarName'{name: "_", source_info: _} % TODO(bladyjoker): Use field index as arg name
           }], TyBody, Ty).

% TODO(bladyjoker): Add handling for record types.

kind_check_module(Sol, Module) :-
    maplist(call(kind_check_ty_def, Sol, Module.module_name), Module.type_defs).

kind_check_ty_def(Sol, ModuleName, '.lambdabuffers.compiler.TyDef'{ ty_name: TyName, ty_body: TyBody, source_info: _ }) :-
    kind_check_ty(ctx(ModuleName, TyName, Sol, []), TyBody, K),
    ctx(ctx(ModuleName, TyName, Sol, []), kind, K).

kind_check_ty_def(Sol, ModuleName, '.lambdabuffers.compiler.TyDef'{ ty_name: TyName, ty_abs: TyAbs, source_info: _ }) :-
    kind_check_ty(ctx(ModuleName, TyName, Sol, []), TyAbs, K),
    ctx(ctx(ModuleName, TyName, Sol, []), kind, K).

ctx(ctx(ModuleName, TyName, Sol, _Args), kind, K) :-
    first(ModuleName-TyName-K, Sol).

ctx(ctx(ModuleName, TyName, _Sol, Args), arg('.lambdabuffers.compiler.TyVar'{
                                                  var_name: _{
                                                                name: VarName,
                                                                source_info: VnSi
                                                            },
                                                  source_info: TvSi
                                              }),K) :-
    first(
        '.lambdabuffers.compiler.TyArg'{
            arg_name: _{name: VarName, source_info: _},
            arg_kind: K
        },
        Args
    ) -> true;
    (
        print_message(error, missing_ty_var(
                                 ModuleName,
                                 TyName,
                                 '.lambdabuffers.compiler.TyVar'{
                                     var_name: _{
                                                   name: VarName,
                                                   source_info: VnSi
                                               },
                                     source_info: TvSi
                                 },
                                 Args)),
        fail
    ).

ctx(ctx(ModuleName, TyName, Sol, _Args), ref('.lambdabuffers.compiler.TyRef'{
                                                  local_ty_ref: _{
                                                                    ty_name: _{
                                                                                 name: LocalTyName,
                                                                                 source_info: LtnSi
                                                                             },
                                                                    source_info: Si
                                                                }
                                              }
                                          ), K) :-
    first(ModuleName-_{name: LocalTyName, source_info:_}-K, Sol) -> true;
    (
        print_message(
            error,
            missing_ty_ref(
                ModuleName,
                TyName,
                '.lambdabuffers.compiler.TyRef'{
                    local_ty_ref: '.lambdabuffers.compiler.TyRef.Local'{
                                      ty_name:  '.lambdabuffers.compiler.TyRef.TyName'{
                                                    name: LocalTyName,
                                                    source_info: LtnSi
                                                },
                                      source_info: Si
                                  }
                },
                Sol)
        ),
        fail
    ).

ctx(ctx(ModuleName, TyName, Sol, _Args), ref(
                                              '.lambdabuffers.compiler.TyRef'{
                                                  foreign_ty_ref: _{
                                                      ty_name: _{
                                                          name: ForeignTyName,
                                                          source_info: FtnSi
                                                      },
                                                      module_name: _{
                                                          name: ForeignModuleName,
                                                          source_info: FmnSi
                                                      },
                                                      source_info: Si
                                                  }
                                              }
                                          ), K) :-
    first(_{name: ForeignModuleName, source_info:_}-_{name: ForeignTyName, source_info: _}-K, Sol) -> true;
    (
        print_message(
            error,
            missing_ty_ref(
                ModuleName,
                TyName,
                '.lambdabuffers.compiler.TyRef'{
                    foreign_ty_ref: {
                        ty_name: {
                            name: ForeignTyName,
                            source_info: FtnSi
                        },
                        module_name: {
                            name: ForeignModuleName,
                            source_info: FmnSi
                        },
                        source_info: Si
                    }
                },
                Sol)
        ),
        fail
    ).

kind_check_ty(Ctx, '.lambdabuffers.compiler.Ty'{ty_var: TyVar}, K) :-
    ctx(Ctx, arg(TyVar), K).

kind_check_ty(Ctx, '.lambdabuffers.compiler.Ty'{ty_ref: TyRef}, K) :-
    ctx(Ctx, ref(TyRef), K).

kind_check_ty(Ctx, '.lambdabuffers.compiler.Ty'{ty_app: _{ty_func: TyFunc, ty_args: TyArgs, source_info: _}}, K) :-
    findall(KArg,
            (
                member(TyArg, TyArgs),
                kind_check_ty(Ctx, TyArg, KArg)
            ),
            KArgs),
     kind_check_ty(Ctx, TyFunc, KAbs),
     kind_arrow_(KArgs, K, KAbs).

kind_check_ty(ctx(Mn, Tn, Rs, As), '.lambdabuffers.compiler.TyAbs'{ty_body: TyBody, ty_args: TyArgs, source_info: _}, K) :-
    findall(KArg,
            (
                member(TyArg, TyArgs),
                KArg=TyArg.arg_kind
            ),
            KArgs),
    kind_check_ty(ctx(Mn, Tn, Rs, [TyArg|As]), TyBody, KBody),
    kind_arrow_(KArgs, KBody, K).

kind_check_ty(_Ctx, '.lambdabuffers.compiler.TyBody'{opaque: _}, K) :-
    kind_ref(type, K).

% Translate to checking for TyName'Sum
kind_check_ty(ctx(Mn, Tn, Rs, As),'.lambdabuffers.compiler.TyBody'{sum: Sum}, K) :-
    findall(CTy,
            (
                member(C, Sum.constructors),
                prod_ty_name(Tn, C.constr_name, ProdTyName),
                ty_local_ref(ProdTyName, LRef),
                CTy='.lambdabuffers.compiler.Ty'{
                       ty_app: '.lambdabuffers.compiler.TyApp'{
                                  ty_func: LRef,
                                  ty_args: C.product.ntuple.fields,
                                  source_info: Sum.source_info
                               }
                   }
            ),
            CTys),
    sum_ty_name(Tn, SumTyName),
    ty_local_ref(SumTyName, LRef),
    kind_check_ty(ctx(Mn, Tn, Rs, As),
                  '.lambdabuffers.compiler.Ty'{
                      ty_app: '.lambdabuffers.compiler.TyApp'{
                                  ty_func: LRef,
                                  ty_args: CTys,
                                  source_info: Sum.source_info
                              }
                  },
                  K).

:- begin_tests(kind_check).

test("module Mod\nopaque Foo :: *", []) :-
    ty_def_opaque("Foo", TyDef),
    mod("Mod", [TyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-(*)
                    ]).

test("module Mod\nopaque Foo a :: * -> *", []) :-
    opaque(TyBody),
    kind_ref(type, KStar),
    ty_arg(a, KStar, TyArg),
    ty_abs([TyArg], TyBody, TyAbs),
    ty_def_abs("Foo", TyAbs, TyDef),
    mod("Mod", [TyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-((*)->(*))
                    ]).

test("module Mod\nopaque Foo f :: (* -> *) -> *", []) :-
    opaque(TyBody),
    kind_ref(type, KStar),
    kind_arrow(KStar, KStar, KArg),
    ty_arg(a, KArg, TyArg),
    ty_abs([TyArg], TyBody, TyAbs),
    ty_def_abs("Foo", TyAbs, TyDef),
    mod("Mod", [TyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-(((*)->(*))->(*))
                    ]).



test("\nmodule Mod\nsum Foo", [ ]) :-
    sum([], TyBody),
    ty_def_body("Foo", TyBody, TyDef),
    mod("Mod", [TyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    print_solution(Solution).

test("\nmodule Mod\nsum Foo = Mk", [ ]) :-
    ntuple([], Prod),
    constr("Mk", Prod, Constr),
    sum([Constr], TyBody),
    ty_def_body("Foo", TyBody, TyDef),
    mod("Mod", [TyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-(*),
                        "Mod"-"Foo'Sum"-((*)->(*)),
                        "Mod"-"Foo'Sum'Mk"-(*)
                    ]).

test("\nmodule Mod\nopaque Int\nsum Foo = Mk Int", [ ]) :-
    ty_def_opaque("Int", IntTyDef),
    ty_local_ref_("Int", IntTyRef),
    ntuple([IntTyRef], Prod),
    constr("Mk", Prod, Constr),
    sum([Constr], FooTyBody),
    ty_def_body("Foo", FooTyBody, FooTyDef),
    mod("Mod", [FooTyDef, IntTyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-(*),
                        "Mod"-"Int"-(*),
                        "Mod"-"Foo'Sum"-((*)->(*)),
                        "Mod"-"Foo'Sum'Mk"-((*)->(*))
                    ]).

test("\nmodule Mod\nopaque Int\nsum Foo = Mk Int String", [ ]) :-
    ty_def_opaque("Int", IntTyDef),
    ty_local_ref_("Int", IntTyRef),
    ty_def_opaque("String", StringTyDef),
    ty_local_ref_("String", StringTyRef),
    ntuple([IntTyRef, StringTyRef], Prod),
    constr("Mk", Prod, Constr),
    sum([Constr], FooTyBody),
    ty_def_body("Foo", FooTyBody, FooTyDef),
    mod("Mod", [FooTyDef, IntTyDef, StringTyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    print_solution(Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-(*),
                        "Mod"-"Int"-(*),
                        "Mod"-"String"-(*),
                        "Mod"-"Foo'Sum"-((*)->(*)),
                        "Mod"-"Foo'Sum'Mk"-((*)->(*)->(*))
                    ]).

test("\nmodule Mod\nopaque Int\nsum Foo = MkA Int String | MkB String ", [ ]) :-
    ty_def_opaque("Int", IntTyDef),
    ty_local_ref_("Int", IntTyRef),
    ty_def_opaque("String", StringTyDef),
    ty_local_ref_("String", StringTyRef),
    ntuple([IntTyRef, StringTyRef], MkAProd),
    constr("MkA", MkAProd, MkAConstr),
    ntuple([StringTyRef], MkBProd),
    constr("MkB", MkBProd, MkBConstr),
    sum([MkAConstr, MkBConstr], FooTyBody),
    ty_def_body("Foo", FooTyBody, FooTyDef),
    mod("Mod", [FooTyDef, IntTyDef, StringTyDef], Mod),
    comp_input([Mod], CompIn),
    kind_check(CompIn, Solution),
    print_solution(Solution),
    pretty_solution(Solution, [
                        "Mod"-"Foo"-(*),
                        "Mod"-"Int"-(*),
                        "Mod"-"String"-(*),
                        "Mod"-"Foo'Sum"-((*)->(*)->(*)),
                        "Mod"-"Foo'Sum'MkA"-((*)->(*)->(*)),
                        "Mod"-"Foo'Sum'MkB"-((*)->(*))
                    ]).

:- end_tests(kind_check).

:- multifile prolog:message//1.

prolog:message(missing_ty_ref(ModuleName, TyName, TyRef, _Sol))
--> {pretty_module_name(ModuleName, Mn)},[
        'Error while kind checking type ~w.~w: Missing local type reference ~w '-[
            Mn, TyName.name, TyRef.local_ty_ref.ty_name.name
        ]
    ].
