:- module(cardinality_check, [first/2, wrap/3, ty_var/1, ty_lam/3]).

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(clpfd)).



ty_var(VarName) :- member(VarName, [a, b, c]).

% Maybe a = Just a | Nothing = a + 1
% Maybe Void = Just Void | Nothing = 0 + 1 = 1
% Maybe Unit = Just Unit | Nothing = 1 + 1 = 2
% Maybe Bool = Just Bool | Nothing = 2 + 1 = 3
% Maybe Tern = Just Tern | Nothing = 3 + 1 = 4
% Either a b = Left a | Right b = a + b
% Tuple a b = Tuple a b = a * b
% 2 -> 3 == 3^2

% Foo a = A a Int (Maybe a) (Maybe Int) | B a

first(X, [H|_]) :-
    X = H.
first(X, [H|Xs]) :-
    X \= H,
    first(X, Xs).

:- dynamic state/2.

ty_lam(Ctx, opaque(kind(K), cardinality(Card)), Lam_) :-
    cardinality_lam(Ctx, Card, Lam),
    wrap(K, Lam, Lam_).

ty_lam(ctx(Vars), ty_ref(RefName), CLam) :-
%    state(RefName, Lam) -> true;
    ty_def(RefName, Ty),
    ty_lam(ctx(Vars), Ty, Lam),
    duplicate_term(Lam, CLam),
    assertz(state(RefName, Lam)).

ty_lam(ctx(Vars), ty_var(VarName), Var) :-
    ty_var(VarName),
    first(VarName-Var, Vars).

ty_lam(ctx(Vars),ty_abs(VarName, TyBody), (LamArg-LamBody)) :-
    LamArg #>= 0,
    ty_lam(ctx([VarName-LamArg|Vars]), TyBody, LamBody).

ty_lam(Ctx, ty_app(TyF, TyArg), LamBody) :-
    ty_lam(Ctx, TyArg, LamArg),
    ty_lam(Ctx, TyF, (LamArg-LamBody)).

cardinality_lam(ctx(Vars), ty_card(ty_var(VarName)), Lam) :-
    writeln(a-Vars),
    first(VarName-Lam, Vars).

cardinality_lam(_, k(Cd), Cd).

cardinality_lam(Ctx, L*R, Cd) :-
    cardinality_lam(Ctx, L, CdL),
    cardinality_lam(Ctx, R, CdR),
    Cd #= CdL * CdR.

cardinality_lam(Ctx, L+R, Cd) :-
    cardinality_lam(Ctx, L, CdL),
    cardinality_lam(Ctx, R, CdR),
    Cd #= CdL + CdR.

wrap(0, V, V).
wrap(N, V, (_-B)) :-
    N \= 0,
    N_ is N - 1,
    wrap(N_, V, B).


foo(list(A), B).
foo(A, list(B)).
