:- module(kind_check, [ty_kind/3, ty_kind/2, kind/1]).

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

ty_kind(ctx(Args, Trace), ty_abs(ArgName-KArg, TyBody), arr(KArg,KBody)) :-
    ty_kind(ctx([ArgName-KArg|Args], Trace), TyBody, KBody).

ty_kind(Ctx, ty_app(TyAbs, TyArg), KRes) :-
    ty_kind(Ctx, TyAbs, KAbs),
    ty_kind(Ctx, TyArg, KArg),
    KAbs = arr(KArg, KRes).

% Example: ty_kind(ty_app(ty_ref(maybe), ty_ref(int8)), K)
ty_kind(Ty, Kind) :-
    ty_kind(ctx([], []), Ty, Kind).

% TODO(bladyjoker): Add iterative deepening.
kind(*).
kind(arr(L, R)) :- kind(L), kind(R).

:- begin_tests(kind_check).

test("should_succeed(Either :: * -> * -> *)", [ nondet ]) :-
    ty_kind(ty_ref(either), arr(*, arr(*, *))).

test("should_succeed(Either :: ?)", [ ]) :-
    aggregate_all(count, ty_kind(ty_ref(either), _), 1).

test("should_succeed(? :: * -> * -> *)", [ ]) :-
    aggregate_all(count, ty_kind(ty_ref(_), arr(*, arr(*, *))), 2).

test("should_succeed(Either Int :: * -> *)", [ nondet ]) :-
    ty_kind(ty_app(ty_ref(either), ty_ref(int8)), arr(*, *)).

test("should_succeed(Either Int :: ?)", [ ]) :-
    aggregate_all(count, ty_kind(ty_app(ty_ref(either), ty_ref(int8)), _), 1).

test("should_succeed(Maybe :: * -> *)", [ nondet ]) :-
    ty_kind(ty_ref(maybe), arr(*, *)).

test("should_succeed(Maybe :: ?)", [ ]) :-
    aggregate_all(count, ty_kind(ty_ref(maybe), _), 1).

test("should_succeed(? :: * -> *)", [ ]) :-
    aggregate_all(count, ty_kind(ty_ref(_), arr(*, *)), 3).

test("should_succeed(Maybe Int :: *)", [ nondet ]) :-
    ty_kind(ty_app(ty_ref(maybe), ty_ref(int8)), *).

test("should_succeed(Maybe Int :: ?)", [ ]) :-
    aggregate_all(count, ty_kind(ty_app(ty_ref(maybe), ty_ref(int8)), *), 1).

test("should_succeed(Maybe ? :: *)", [ ]) :-
    aggregate_all(count, ty_kind(ty_app(ty_ref(maybe), ty_ref(_)), *), 9).

test("should_succeed(Int :: *)", [ nondet ]) :-
    ty_kind(ty_ref(int8), *).

test("should_succeed(Int :: ?)", [ ]) :-
    aggregate_all(count, ty_kind(ty_ref(int8), *), 1).

test("should_succeed(? :: *)", [ ]) :-
    aggregate_all(count, ty_kind(ty_ref(_), *), 9).

test("should_succeed(List :: * -> *)", [ nondet ]) :-
    ty_kind(ty_ref(list), arr(*,*)).

test("should_succeed(Rec :: * -> *)", [ nondet ]) :-
    ty_kind(ty_ref(rec), arr(*,*)).

test("should_succeed(RecFoo :: *)", [ nondet ]) :-
    ty_kind(ty_ref(recfoo), *).

% TODO(bladyjoker): Hmm...
test("should_fail(Either ? :: *)", [ ]) :-
    true.

:- end_tests(kind_check).
