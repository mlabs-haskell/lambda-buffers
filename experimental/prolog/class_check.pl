:- use_module(common_defs).
:- use_module(kind_check).

%% Apply and argument to a type abstraction - assumes Kind validity
% Beta-reduction Apply an argument to a type abstraction - assumes Kind validity
apply(opaque(N, kind(arr(_KL, KR)), Cd), _, opaque(N, kind(KR), Cd)).

apply(ty_ref(RefName), A, ResTy) :-
    ty_def(RefName, Ty),
    apply(Ty, A, ResTy).

apply(ty_app(L, R), A, Res) :-
    apply(L, R, ResApp),
    apply(ResApp, A, Res).

apply(ty_abs(ArgName-_, Body), A, Res) :-
    subst(ctx(ArgName-A, []), Body, Res).

subst(ctx(VarName-A, Args), ty_var(VarName), Res) :-
    first(VarName, Args) -> Res = ty_var(VarName);
    Res = A.

subst(ctx(ArgName-_, _), ty_var(VarName), ty_var(VarName)) :-
    ArgName \= VarName.

subst(_, opaque(N, K, Cd), opaque(N, K, Cd)).

subst(_, ty_ref(RefName), ty_ref(RefName)).

subst(ctx(Arg, Args), ty_abs(ArgName-KArg, Body), ty_abs(ArgName-KArg, Res)) :-
    subst(ctx(Arg, [ArgName|Args]), Body, Res).

subst(Ctx, ty_app(TyAbs, TyArg), ty_app(AbsRes, ArgRes)) :-
    subst(Ctx, TyAbs, AbsRes),
    subst(Ctx, TyArg, ArgRes).

%% Sum/Product/Rec get normalized into a canonical form Either/Prod/Void/Unit/Opaque
%% Structural rules for types of kind `*`
struct_rule(class(ClassName, class_arg(_, kind(*)), _), Rule) :-
    member(Rule, [
               (rule(ClassName, opaque(_, kind(*), _)) :- true),
               (rule(ClassName, ty_ref(unit)) :- true),
               (rule(ClassName, ty_ref(void)) :- true),
               (rule(ClassName, ty_app(ty_app(ty_ref(prod), A), B)) :-
                    (
                        rule(ClassName, A),
                        rule(ClassName, B)
                    )),
               (rule(ClassName, ty_app(ty_app(ty_ref(either), A), B)) :-
                    (
                        rule(ClassName, A),
                        rule(ClassName, B)
                    ))
           ]).

conj(Goal, (Goal, Conj), Conj).

superclass_goals(Ty, Cl_, Conj) :-
    copy_term(Cl_, Cl),
    class(_ClassName, class_arg(Ty, _K), ClassSups) = Cl,
    findall(R, (
                member(Sup_, ClassSups),
                copy_term(Sup_, Sup),
                Sup =.. [SupName, Ty],
                R = rule(SupName, Ty)
            ),
            Rules),
    foldl(conj, Rules, Conj, true).


%% User specifiable `derive` rules (the same for any kind?)
%% NOTE(bladyjoker): TyAbs can't be derived for non `*` kinds.
derive_rule(ty_ref(RefName), class(ClassName, ClassArgs, ClassSups), Rule) :-
    ty_def(RefName, Ty),
    superclass_goals(ty_ref(RefName), class(ClassName, ClassArgs, ClassSups), SupGoals),
    Rule = (rule(ClassName, ty_ref(RefName)) :- rule(ClassName, Ty), SupGoals).

derive_rule(ty_app(F, A), class(ClassName, ClassArgs, ClassSups), Rule) :-
    apply(F, A, Res),
    superclass_goals(ty_app(F, A), class(ClassName, ClassArgs, ClassSups), SupGoals),
    Rule =  (rule(ClassName, ty_app(F, A)) :- rule(ClassName, Res), SupGoals).

%% Experimental structural rules for types of kind * -> *
% Haskell: Functor Deriving https://mail.haskell.org/pipermail/haskell-prime/2007-March/002137.html
%% struct_rule(class(ClassName, class_arg(_, kind(arr(_KL, _KR))), _), Rule) :-
%%     member(Rule, [
%%                (rule(ClassName, ty_app(TL, TR)) :-
%%                     (
%%                         rule(ClassName, TL),
%%                         apply(TL, TR, Res),
%%                         rule(ClassName, Res)
%%                     )),
%%                (rule(ClassName, ty_abs(_A, B)) :-
%%                     (
%%                         rule(ClassName, B)
%%                     ))
%%            ]
%%           ).



class_def(eq, class_arg(_A, kind(*)), []).
class_def(ord, class_arg(A, kind(*)), [eq(A)]).
class_def(json, class_arg(_A, kind(*)), []).
class_def(functor, class_arg(_A, kind(arr(*, *))), []).


derive(Tys, CName, StructRules, UserRules) :-
    class_def(CName, CArg, CSups),
    findall(StructRule,
            (
                struct_rule(
                    class(CName, CArg, CSups),
                    StructRule
                )
            ),
            StructRules
           ),
    findall(UserRule,
            (
                member(Ty, Tys),
                derive_rule(
                    Ty,
                    class(CName, CArg, CSups),
                    UserRule
                )
            ),
            UserRules
           ).

solve(StructRules, UserRules, Goal) :-
    Goal =.. [ClassName, Ty],
    append(StructRules, UserRules, Rules),
    eval_rule(Rules, [], rule(ClassName, Ty)) -> true;
        (
            print_message(error, rule_failed(Goal)),
            fail
        ).

eval_rule(_, _, true) :-
    print_message(informational, rule_true).

eval_rule(Rules, Trace, (RL,RR)) :-
    eval_rule(Rules, Trace, RL),
    eval_rule(Rules, Trace, RR).

eval_rule(Rules, Trace, rule(ClassName, Ty)) :-
    var(Ty) -> print_message(informational, rule_ok(rule(ClassName, Ty))), true;
    copy_term(Trace, Trace_), %% WARN(bladyjoker): Without this, Trace gets unified and instantiated.
    first(rule(ClassName, Ty), Trace_) -> print_message(informational, rule_ok_cycle(rule(ClassName, Ty, Trace))), true;
    (
        print_message(informational, lookup(rule(ClassName, Ty))),
        copy_term(Rules, Rules_), %% WARN(bladyjoker): Without this, Rules get unified and instantiated leading to a cycle and just wrong.
        first((rule(ClassName, Ty) :- RuleBody), Rules_) -> (
            print_message(informational, trying(rule(ClassName, Ty))),
            eval_rule(Rules, [rule(ClassName, Ty)|Trace], RuleBody),
            print_message(informational, rule_ok(rule(ClassName, Ty)))
        );
        (
            print_message(error, missing_rule(rule(ClassName, Ty), Trace)),
            fail
        )
    ).

:- multifile prolog:message//1.

prolog:message(wrong_kind(Ty, got(Got), wanted(Want))) --> {pretty_ty(Ty, PTy)}, ['~w is of kind ~w but wanted kind ~w'-[PTy, Got, Want]].
prolog:message(normalization_failed(_, Ty)) --> {pretty_ty(Ty, PTy)}, ['Normalizing ~w failed'-[PTy]].
prolog:message(lookup(rule(ClassName, Ty))) --> {pretty_ty(Ty, PTy)}, ['Looking up rule ~w ~w'-[ClassName, PTy]].
prolog:message(trying(rule(ClassName, Ty))) --> {pretty_ty(Ty, PTy)}, ['Trying rule ~w ~w'-[ClassName, PTy]].
prolog:message(rule_ok(rule(ClassName, Ty))) --> {pretty_ty(Ty, PTy)}, ['Done with rule ~w ~w'-[ClassName, PTy]].
prolog:message(rule_ok_cycle(rule(ClassName, Ty, Trace))) --> {pretty_ty(Ty, PTy), pretty_trace(Trace, PTrace)}, ['Done with rule because cycle ~w ~w ~w'-[ClassName, PTy, PTrace]].
prolog:message(rule_true) --> [ 'Done because bottom'].
prolog:message(missing_rule(rule(ClassName, Ty), _)) --> {pretty_ty(Ty, PTy)}, ['Missing rule ~w ~w'-[ClassName, PTy]].
prolog:message(rule_failed(rule(ClassName, Ty))) --> {pretty_ty(Ty, PTy)}, ['Failed rule ~w ~w'-[ClassName, PTy]].

%% Pretty represenationts
%% ?- pretty_ty(ty_app(ty_app(ty_ref(either), ty_ref(int)), B), P).
%% P = either(int, B).
pretty_ty(TyVar, TyVar) :-
    var(TyVar).
pretty_ty(ty_ref(RefName), P) :-
    P =.. [RefName].
pretty_ty(ty_app(TyF, TyA), P) :-
    (var(TyF) -> PTyF = TyF; pretty_ty(TyF, PTyF)),
    (var(TyA) -> PTyA = TyA; pretty_ty(TyA, PTyA)),
    PTyF =.. [N|Args],
    append(Args, [PTyA], PArgs),
    P =.. [N|PArgs].

pretty_rule(rule(ClassName, Ty), P) :-
    pretty_ty(Ty, PTy),
    P =.. [ClassName, PTy].

pretty_trace(Trace, PTrace) :-
    findall(P, (member(R, Trace), pretty_rule(R, P)), PTrace).

:- begin_tests(class_check).

test("should_succeed: derive Eq Int)", []) :-
    derive([ty_ref(int8)], eq, S, U),
    solve(S, U, eq(ty_ref(int8))).

test("should_succeed: derive Eq Maybe Int8)", []) :-
    derive([ty_ref(int8), ty_app(ty_ref(maybe), ty_ref(int8))], eq, S, U),
    solve(S, U, eq(ty_ref(int8))),
    solve(S, U, eq(ty_app(ty_ref(maybe), ty_ref(int8)))).

test("should_succeed: derive Eq (Maybe a))", []) :-
    derive([ty_app(ty_ref(maybe), _A)], eq, S, U),
    solve(S, U, eq(ty_app(ty_ref(maybe), _B))).

test("should_fail(derive Eq Foo)", [ fail ]) :-
    derive([ty_ref(foo)], eq, S, U),
    solve(S, U, eq(ty_ref(foo))).

test("should_fail(derive Eq Foo; derive Eq Int8)", [ fail ]) :-
    derive([ty_ref(int8), ty_ref(foo)], eq, S, U),
    solve(S, U, eq(ty_ref(int8))),
    solve(S, U, eq(ty_ref(foo))).

test("should_succeed: derive Eq Foo; derive Eq Int8)", [ ]) :-
    derive([
                  ty_ref(int8),
                  ty_ref(foo),
                  ty_app(ty_ref(maybe), _A)
              ], eq, S, U),
    solve(S, U, eq(ty_ref(int8))),
    solve(S, U, eq(ty_ref(foo))),
    solve(S, U, eq(ty_app(ty_ref(maybe), _B))).

test("should_fail: derive Eq Recfoo", [ fail ]) :-
    derive([
                  ty_ref(recfoo)
              ], eq, S, U),
    solve(S, U, eq(ty_ref(recfoo))).

test("should_succeed: derive Eq Recfoo, Eq Recbar)", [ ]) :-
    derive([
                  ty_ref(recfoo),
                  ty_ref(recbar)
              ], eq, S, U),
    solve(S, U, eq(ty_ref(recfoo))).

test("should_succeed: derive Eq Maybe (Maybe Int8))", [ ]) :-
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), _A)
              ], eq, S, U),
    solve(S, U, eq(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))))).

test("should_succeed: derive Eq Maybe (Maybe a)", [ ]) :-
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), _A)
              ], eq, S, U),
    solve(S, U, eq(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), _B)))).

test("should_fail: derive Ord (Maybe Int)", [ fail ]) :-
    derive([
                  ty_app(ty_ref(maybe), _A)
              ], ord, S, U),
    solve(S, U, ord(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))))).

test("should_fail: derive Ord (Maybe Int)", [ fail ]) :-
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), _A)
              ], ord, S, U),
    solve(S, U, ord(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))))).

test("should_succeed: derive Ord (Maybe a)", [ fail ]) :-
    derive([
                  ty_ref(int8)
              ], eq, EqS, EqU),
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), __A)
              ], ord, OrdS, OrdU),
    append(EqS, OrdS, S),
    append(EqU, OrdU, U),
    solve(S, U, ord(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))))).

test("should_succeed: derive Ord (Maybe a)", [ ]) :-
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), __A)
              ], eq, EqS, EqU),
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), __A)
              ], ord, OrdS, OrdU),
    append(EqS, OrdS, S),
    append(EqU, OrdU, U),
    solve(S, U, ord(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))))).

:- end_tests(class_check).
