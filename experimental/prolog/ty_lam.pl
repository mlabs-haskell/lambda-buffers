% Module that translates a Ty expression into a Lambda expression for convenient evaluation.
% Assumes proper kinds.
% Lambda evaluation is simply normalization that ultimately yields lambdas and 'applied' opaques.
% Doesn't work with rec types, this is strict eval.
:- module(ty_lam, [ty_lam/3, ty_lam/2]).

:- use_module(common_defs).
:- use_module(kind_check).

% TODO(bladyjoker): Generalize kind(*)
opq_lam(opaque(N, kind(*), _), Args, opaque(N, Args)).

opq_lam(opaque(N, kind(arr(_, KR)), Cd), Args, Lam) :-
    opq_lam(opaque(N, kind(KR), Cd),
            Args_,
            L
           ),
    Lam = ({L, Args, Args_}/[A, L]>>(Args_ = [A|Args])).

ty_lam(_, opaque(N, K, Cd), Lam) :-
    opq_lam(opaque(N, K, Cd), [], Lam).

ty_lam(ctx(Vars, Trace), ty_ref(RefName), Lam) :-
    first(RefName-Lam, Trace) -> true;
    ty_def(RefName, FTy),
    ty_lam(ctx(Vars, [RefName-Lam|Trace]), FTy, Lam).

ty_lam(ctx(Vars, _), ty_var(VarName), Lam) :-
    first(VarName-Lam, Vars).

ty_lam(ctx(Vars, Trace), ty_abs(ArgName-_, TyBody), Lam)  :-
    ty_lam(ctx([ArgName-Arg|Vars], Trace), TyBody, Res),
    Lam = ({Arg, Res}/[Arg, Res]>>true).

ty_lam(Ctx, ty_app(F, A), Lam)  :-
    ty_lam(Ctx, F, FLam),
    ty_lam(Ctx, A, ALam),
    call(FLam, ALam, Lam).

ty_lam(Ty, Lam) :-
    empty_ctx(Ctx),
    ty_lam(Ctx, Ty, Lam).

empty_ctx(ctx([], [])).

:- begin_tests(ty_lam).

test("should_succeed -> Either Int8 Int8", [ ]) :-
    ty_lam(
        ty_app(ty_app(ty_ref(either), ty_ref(int8)), ty_ref(int8)),
        opaque(either, [opaque(int8, []), opaque(int8, [])])
    ).

test("should_succeed -> (\\a -> a) bytes", [
     ]) :-
    ty_lam(
        ty_app(ty_abs(a-(*), ty_var(a)), ty_ref(bytes)),
        opaque(bytes, [])
    ).

test("should_fail -> bytes int", [
         fail
     ]) :-
    ty_lam(ty_app(ty_ref(bytes), ty_ref(int)), _).

test("should_succeed -> (\\a :: * -> (\\a :: * -> a) a) int8", [  ]) :-
    ty_lam(ty_app(ty_abs(a-(*), ty_app(ty_abs(a-(*), ty_var(a)), ty_var(a))), ty_ref(int8)), opaque(int8, [])).

test("should_succeed -> Maybe (Maybe Int8)", [  ]) :-
    ty_lam(
        ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))),
        opaque(either, [opaque(either, [opaque(int8, []), opaque(unit, [])]), opaque(unit, [])])
    ).

:- end_tests(ty_lam).
