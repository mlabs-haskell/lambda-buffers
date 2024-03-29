:-module(proto_aux, [
             read_compiler_input/2,
             comp_input/2,
             mod/3,
             ty_def_body/3,
             ty_def_abs/3,
             ty_local_ref/2,
             ty_local_ref_/2,
             ty_foreign_ref/3,
             ty_foreign_ref_/3,
             ty_app/3,
             ty_abs/3,
             ty_arg/3,
             ty_var/2,
             ty_def_opaque/2,
             ntuple/2,
             sum/2,
             opaque/1,
             constr/3,
             kind_ref/2,
             kind_arrow/3,
             kind_arrow_/3,
             pretty_solution/2,
             print_solution/1,
             pretty_kind/2,
             pretty_module_name/2,
             pretty_foreign_ty_ref/2
         ]).

:- use_module(compiler_pb).
:- use_module(library(protobufs)).

read_compiler_input(Filename, Res) :-
    read_file_to_codes(Filename, Cs, []),
    protobuf_parse_from_codes(Cs, '.lambdabuffers.compiler.CompilerInput', Res).

% Helpers
source_info(File, From, To, '.lambdabuffers.compiler.SourceInfo'{
                                file: File,
                                pos_from: From,
                                pos_to: To
                            }).

ty_local_ref(TyName, '.lambdabuffers.compiler.Ty'{
                         ty_ref: '.lambdabuffers.compiler.TyRef'{
                                     local_ty_ref: '.lambdabuffers.compiler.TyRef.Local'{
                                                       ty_name: TyName,
                                                       source_info: _
                                                   }
                                 }
                     }).

ty_local_ref_(TyName, Ty) :-
    ty_local_ref('.lambdabuffers.compiler.TyName'{ name: TyName, source_info: _}, Ty).

ty_foreign_ref(ModName, TyName, '.lambdabuffers.compiler.Ty'{
                                    ty_ref: '.lambdabuffers.compiler.TyRef'{
                                                foreign_ty_ref: '.lambdabuffers.compiler.TyRef.Foreign'{
                                                                    ty_name: TyName,
                                                                    module_name: ModName,
                                                                    source_info: _
                                                                }
                                            }
                                }).

ty_foreign_ref_(ModName, TyName, Ty) :-
    findall('.lambdabuffers.compiler.ModuleNamePart'{ name: P, source_info: _}, member(P, ModName), Ps),
    ty_foreign_ref(
        '.lambdabuffers.compiler.ModuleName'{ parts: Ps, source_info: _},
        '.lambdabuffers.compiler.TyName'{ name: TyName, source_info: _},
        Ty).


ty_app(TyF, TyArgs, '.lambdabuffers.compiler.Ty'{
                       ty_app: '.lambdabuffers.compiler.TyApp'{
                                   ty_func: TyF,
                                   ty_args: TyArgs,
                                   source_info: _Si % TODO(bladyjoker): Missing source info
                               }
                   }).

% gdebug, read_compiler_input('compiler-input.pb', CompIn), kind_check(CompIn, Res), print_solution(Res).
ty_abs(TyArgs, TyBody, '.lambdabuffers.compiler.TyAbs'{
                          ty_args: TyArgs,
                          ty_body: TyBody,
                          source_info: _Si % TODO(bladyjoker): Missing source info
                      }).

ty_var(VarName, '.lambdabuffers.compiler.Ty'{ ty_var: '.lambdabuffers.compiler.TyVar'{
                                                          var_name: '.lambdabuffers.compiler.VarName'{ name: VarName, source_info: _},
                                                          source_info: _
                                                      }}).

comp_input(Modules, '.lambdabuffers.compiler.CompilerInput'{ modules: Modules }).

mod(ModuleName, TyDefs, '.lambdabuffers.compiler.Module'{
                               module_name: '.lambdabuffers.compiler.ModuleName'{
                                                parts: ['.lambdabuffers.compiler.ModuleNamePart'{
                                                            name: ModuleName,
                                                            source_info: _
                                                        }],
                                                source_info: _
                                            },
                               source_info: _,
                               type_defs: TyDefs
                           }).

ty_def_body(TyName, TyBody, '.lambdabuffers.compiler.TyDef'{
                           ty_name: '.lambdabuffers.compiler.TyName'{ name: TyName, source_info: _ },
                           ty_body: TyBody,
                           source_info: _
                       }).

ty_def_abs(TyName, TyAbs, '.lambdabuffers.compiler.TyDef'{
                           ty_name: '.lambdabuffers.compiler.TyName'{ name: TyName, source_info: _ },
                           ty_abs: TyAbs,
                           source_info: _
                       }).

opaque('.lambdabuffers.compiler.TyBody'{ opaque: '.lambdabuffers.compiler.Opaque'{ source_info: _ }}).

sum(Cons, '.lambdabuffers.compiler.TyBody'{ sum: '.lambdabuffers.compiler.Sum'{ constructors: Cons, source_info: _ }}).

constr(ConstrName, Prod, '.lambdabuffers.compiler.Constructor'{
                             constr_name: '.lambdabuffers.compiler.ConstrName'{
                                              name: ConstrName,
                                              source_info: _},
                             product: Prod,
                             source_info: _ }).

ntuple(Fields,  '.lambdabuffers.compiler.Product'{
                    ntuple: '.lambdabuffers.compiler.Product.NTuple'{
                                fields: Fields,
                                source_info: _
                            },
                    source_info: _
                }).

ty_arg(ArgName, ArgKind, '.lambdabuffers.compiler.TyArg'{
                             arg_name: '.lambdabuffers.compiler.VarName'{ name: ArgName, source_info: _ },
                             arg_kind: ArgKind,
                             source_info: _
                         }).

ty_def_opaque(TyName, TyDef) :-
    opaque(TyBody),
    ty_def_body(TyName, TyBody, TyDef).



kind_arrow(Left, Right, '.lambdabuffers.compiler.Kind'{ kind_arrow: '.lambdabuffers.compiler.Kind.KindArrow'{left: Left, right: Right} }).

kind_ref(type, '.lambdabuffers.compiler.Kind'{ kind_ref: 'KIND_REF_TYPE' }).

kind_arrow_([], K, K).
kind_arrow_([KArg|KArgs], K, KAbs) :-
    kind_arrow_(KArgs, K, KAbs_),
    kind_arrow(KArg, KAbs_, KAbs).


module_name_parts_tolist([], []).
module_name_parts_tolist([P|Ps], [P.name|Ps_]) :-
    module_name_parts_tolist(Ps, Ps_).


pretty_solution([], []).
pretty_solution([ModuleName-TyName-K|Rs], [Mn-TyName.name-K_|Rs_]) :-
    pretty_module_name(ModuleName, Mn),
    pretty_kind(K, K_),
    pretty_solution(Rs, Rs_).

pretty_kind(K, K_) :-
    var(K) -> K_='?';
    (
        K = '.lambdabuffers.compiler.Kind'{kind_ref: 'KIND_REF_TYPE'} -> K_='*'
    ).
pretty_kind(K, L_->R_) :-
    \+var(K) -> K = '.lambdabuffers.compiler.Kind'{kind_arrow: '.lambdabuffers.compiler.Kind.KindArrow'{left: L, right: R}},
                pretty_kind(L, L_),
                pretty_kind(R, R_).

pretty_module_name(ModuleName, Mn) :-
    module_name_parts_tolist(ModuleName.parts, Ps),
    atomics_to_string(Ps, ".", Mn).

print_solution(Solution) :-
    pretty_solution(Solution, Solution_),
    maplist(writeln, Solution_).

pretty_foreign_ty_ref(FTyRef, FTyRef_) :-
    pretty_module_name(FTyRef.module_name, Mn),
    atomics_to_string([Mn, FTyRef.ty_name.name], ".", FTyRef_).
