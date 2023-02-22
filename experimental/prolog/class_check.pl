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
               (goal(ClassName, opaque(_, kind(*), _)) :- true),
               (goal(ClassName, ty_ref(unit)) :- true),
               (goal(ClassName, ty_ref(void)) :- true),
               (goal(ClassName, ty_app(ty_app(ty_ref(prod), A), B)) :-
                    (
                        goal(ClassName, A),
                        goal(ClassName, B)
                    )),
               (goal(ClassName, ty_app(ty_app(ty_ref(either), A), B)) :-
                    (
                        goal(ClassName, A),
                        goal(ClassName, B)
                    ))
           ]).

conj(Goal, (Goal, Conj), Conj).

superclass_goal(Ty, Cl_, Conj) :-
    copy_term(Cl_, Cl),
    class(_ClassName, class_arg(Ty, _K), ClassSups) = Cl,
    findall(R, (
                member(Sup_, ClassSups),
                copy_term(Sup_, Sup),
                Sup =.. [SupName, Ty],
                R = goal(SupName, Ty)
            ),
            Rules),
    foldl(conj, Rules, Conj, true).


%% User specifiable `derive` rules (the same for any kind?)
%% NOTE(bladyjoker): TyAbs can't be derived for non `*` kinds.
derive_rule(ty_ref(RefName), class(ClassName, ClassArgs, ClassSups), Rule) :-
    ty_def(RefName, Ty),
    superclass_goal(ty_ref(RefName), class(ClassName, ClassArgs, ClassSups), SupGoals),
    Rule = (goal(ClassName, ty_ref(RefName)) :- goal(ClassName, Ty), SupGoals).

derive_rule(ty_app(F, A), class(ClassName, ClassArgs, ClassSups), Rule) :-
    apply(F, A, Res),
    superclass_goal(ty_app(F, A), class(ClassName, ClassArgs, ClassSups), SupGoals),
    Rule =  (goal(ClassName, ty_app(F, A)) :- goal(ClassName, Res), SupGoals).

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
    solve_goal(Rules, [], goal(ClassName, Ty)) ->( true;
                                                  print_message(error, goal_failed(goal(ClassName, Ty))),
                                                  fail
                                                ).

solve_goal(_, Trace, true) :-
    print_message(informational, goal_true(Trace)).

solve_goal(Rules, Trace, (GL,GR)) :-
    solve_goal(Rules, Trace, GL),
    solve_goal(Rules, Trace, GR).

solve_goal(Rules, Trace, goal(ClassName, Ty)) :-
    var(Ty) -> print_message(informational, goal_ok(goal(ClassName, Ty), Trace)), true;
    check_cycle(Trace, goal(ClassName, Ty)) -> true;
    (
        print_message(informational,
                      lookup(goal(ClassName, Ty), Trace)
                     ),
        copy_term(Rules, Rules_), %% WARN(bladyjoker): Without this, Rules get unified and instantiated leading to a cycle and just wrong.
        first((goal(ClassName, Ty) :- RuleBody), Rules_) -> (
            print_message(informational,
                          running(goal(ClassName, Ty), Trace)
                         ),
            solve_goal(Rules, [goal(ClassName, Ty)|Trace], RuleBody),
            print_message(informational,
                          goal_ok(goal(ClassName, Ty), Trace)
                         )
        );
        (
            print_message(error, missing_rule(goal(ClassName, Ty), Trace)),
            fail
        )
    ).

check_cycle(Trace, Goal) :-
    copy_term(Trace, Trace_), %% WARN(bladyjoker): Without this, Trace gets unified and instantiated.
    print_message(informational, checking_cycle(Goal, Trace)),
    (member(TracedGoal, Trace_), TracedGoal =@= Goal) -> print_message(informational, goal_ok_cycle(Goal, Trace)); fail.


:- multifile prolog:message//1.

trace_to_indentation([], "").
trace_to_indentation([_|Xs], I) :-
    trace_to_indentation(Xs, Is),
    string_concat(".", Is, I).

prolog:message(checking_cycle(G, Trace)) --> {
                                         trace_to_indentation(Trace, I),
                                         pretty_goal(G, PG)
                                     }, [
                                         '~w Checking cycle for for goal ~w '-[I, PG]
                                     ].

prolog:message(lookup(G, Trace)) --> {
                                         trace_to_indentation(Trace, I),
                                         pretty_goal(G, PG)
                                     }, [
                                         '~w Looking up rule for goal ~w '-[I, PG]
                                     ].
prolog:message(running(G, Trace)) --> {
                                                           trace_to_indentation(Trace, I),
                                                           pretty_goal(G, PG)
                                                       }, [
                                                           '~w Running goal ~w '-[I, PG]
                                                       ].
prolog:message(goal_ok(G, Trace)) --> {
                                          trace_to_indentation(Trace, I),
                                          pretty_goal(G, PG)
                                      }, [
                                          '~w Done with goal ~w'-[I, PG]
                                      ].
prolog:message(goal_ok_cycle(G, Trace)) --> {
                                                trace_to_indentation(Trace, I),
                                                pretty_goal(G, PG),
                                                pretty_trace(Trace, PTrace)
                                            }, [
                                                '~w Done with goal because cycle ~w ~w '-[I, PG, PTrace]
                                            ].
prolog:message(goal_true(Trace)) --> { trace_to_indentation(Trace, I) }, [ '~w Done because bottom'-[I]].
prolog:message(missing_rule(G, Trace)) --> {
                                               trace_to_indentation(Trace, I),
                                               pretty_goal(G, PG)
                                           }, [
                                               '~w Missing rule for goal ~w'-[I, PG]
                                           ].
prolog:message(goal_failed(G)) --> {pretty_goal(G, PG)}, ['Failed goal ~w'-[PG]].

%% Pretty represenationts
%% ?- pretty_ty(ty_app(ty_app(ty_ref(either), ty_ref(int)), B), P).
%% P = either(int, B).
pretty_ty(TyVar, TyVar) :-
    var(TyVar).
pretty_ty(opaque(N, _, _), P) :-
    atom_concat('_', N, OpaqueN),
    P =.. [OpaqueN].
pretty_ty(ty_ref(RefName), P) :-
    P =.. [RefName].
pretty_ty(ty_app(TyF, TyA), P) :-
    (var(TyF) -> PTyF = TyF; pretty_ty(TyF, PTyF)),
    (var(TyA) -> PTyA = TyA; pretty_ty(TyA, PTyA)),
    PTyF =.. [N|Args],
    append(Args, [PTyA], PArgs),
    P =.. [N|PArgs].

pretty_goal(goal(ClassName, Ty), P) :-
    pretty_ty(Ty, PTy),
    P =.. [ClassName, PTy].

pretty_trace(Trace, PTrace) :-
    findall(P, (member(R, Trace), pretty_goal(R, P)), PTrace).

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
                  ty_app(ty_ref(maybe), _A)
              ], eq, EqS, EqU),
    derive([
                  ty_ref(int8),
                  ty_app(ty_ref(maybe), __A)
              ], ord, OrdS, OrdU),
    append(EqS, OrdS, S),
    append(EqU, OrdU, U),
    solve(S, U, ord(ty_app(ty_ref(maybe), ty_app(ty_ref(maybe), ty_ref(int8))))).

test("should_fails: Eq List a => Eq List a", [ ]) :-
    solve([
                 (
                     goal(eq, ty_app(ty_ref(list), A)) :-
                         (goal(eq, ty_app(ty_ref(list), A)),true)
                 )
             ],
          [],
          eq(ty_app(ty_ref(list), _B))
         ).

:- end_tests(class_check).
