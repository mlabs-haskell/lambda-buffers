:- module(ty_lam, [ty_lam/3, ty_lam/2]).

:- use_module(common_defs).

% TODO(bladyjoker): Generalize kind(*), evaluate Cardinality under ctx
opq_lam(_, opaque(kind(*), Cd), opaque([], Cd)).

opq_lam(Ctx, opaque(kind(arr(_, KR)), Cd), Lam) :-
    opq_lam(Ctx,
            opaque(kind(KR), Cd),
            L
           ),
    Lam = ({L}/[A, L]>>true).

ty_lam(Ctx, opaque(K, Cd), Lam) :-
    opq_lam(Ctx, opaque(K, Cd), Lam).

ty_lam(Ctx, ty_ref(F), Lam) :-
    ty_def(F, FTy),
    ty_lam(Ctx, FTy, Lam).

ty_lam(ctx(Vars), ty_var(VarName), Lam) :-
    first(VarName-Lam, Vars).

ty_lam(ctx(Vars), ty_abs(ArgName-_, TyBody), Lam)  :-
    ty_lam(ctx([ArgName-Arg|Vars]), TyBody, Res),
    Lam = ({Arg, Res}/[Arg, Res]>>true).

ty_lam(Ctx, ty_app(F, A), Lam)  :-
    ty_lam(Ctx, F, FLam),
    ty_lam(Ctx, A, ALam),
    call(FLam, ALam, Lam).

ty_lam(Ty, Lam) :-
    empty_ctx(Ctx),
    ty_lam(Ctx, Ty, Lam).

empty_ctx(ctx([])).
