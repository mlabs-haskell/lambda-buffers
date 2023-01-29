:- module(kind_check, [ty_kind/3, ty_kind/2, kind/1]).

:- use_module(library(clpfd)).
:- use_module(common_defs).

% TODO(bladyjoker): Provide a proper out moded var_name pred
ty_var_name(VarName) :- member(VarName, [a, b, c]).

ty_kind(_, opaque(kind(K), _), K).

ty_kind(ctx(Vars,Trace), ty_ref(RefName), K) :-
    first(RefName-K, Trace) -> true;
    (
        ty_def(RefName, Ty),
        ty_kind(ctx(Vars, [RefName-K|Trace]), Ty, K)
    ).

ty_kind(ctx(Args, _), ty_var(VarName), KArg) :-
    ty_var_name(VarName),
    first(VarName-KArg, Args).

ty_kind(ctx(Args, Trace), ty_abs(ArgName-KArg, TyBody), KAbs) :-
    append([ArgName-KArg], Args, Args_),
    ty_kind(ctx(Args_, Trace), TyBody, KBody),
    KAbs = (KArg -> KBody).

ty_kind(Ctx, ty_app(TyAbs, TyArg), KRes) :-
    ty_kind(Ctx, TyArg, KArg),
    ty_kind(Ctx, TyAbs, KAbs),
    KAbs = (KArg -> KRes).

% Example: ty_kind(ty_app(ty_ref(maybe), ty_ref(int8)), K)
ty_kind(Ty, Kind) :-
    ty_kind(ctx([], []), Ty, Kind).

% TODO(bladyjoker): Add iterative deepening.
kind(*).
kind(L -> R) :- kind(L), kind(R).
