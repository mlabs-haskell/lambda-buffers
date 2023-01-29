%% Simple arity based kind checking (only * KindRefs and all TyArgs are of arity 0)
:- module(kind_check, [ty_kind/3, ty_kind/2]).

:- use_module(library(clpfd)).
:- use_module(common_defs).

ty_var_name(VarName) :- member(VarName, [a, b, c]).

ty_kind(_, opaque(kind(K), _), K).

ty_kind(Ctx, ty_ref(RefName), K) :-
    ty_def(RefName, Ty),
    ty_kind(Ctx, Ty, K).

ty_kind(ctx(Args), ty_var(VarName), KArg) :-
    ty_var_name(VarName), %% TODO(bladyjoker): Provide a proper out moded var_name pred
    first(VarName-KArg, Args).

ty_kind(ctx(Args), ty_abs(ArgName, TyBody), KtAbs) :-
    KArg #= 0,
    append([ArgName-KArg], Args, Args_),
    ty_kind(ctx(Args_), TyBody, KBody),
    KtAbs #= 1 + KBody.

ty_kind(Ctx, ty_app(TyFun, TyArg), KRes) :-
    ty_kind(Ctx, TyArg, KArg),
    ty_kind(Ctx, TyFun, KFun),
    KRes #= KFun - 1.

ty_kind(Ty, Kind) :-
    ty_kind(ctx([]), Ty, Kind).
