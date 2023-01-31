:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(clpfd)).
:- use_module(common_defs).
:- use_module(kind_check).

%% Apply and argument to a type abstraction - assumes Kind validity
%% TODO(bladyjoker): Just use the Lambda representation of types, it's easier.
apply(ty_ref(RefName), A, Res) :-
    ty_def(RefName, Ty),
    apply(Ty, A, Res).

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

subst(_, opaque(K, Cd), opaque(K, Cd)).

subst(_, ty_ref(RefName), ty_ref(RefName)).

subst(ctx(Arg, Args), ty_abs(ArgName-KArg, Body), ty_abs(ArgName-KArg, Res)) :-
    subst(ctx(Arg, [ArgName|Args]), Body, Res).

subst(Ctx, ty_app(TyAbs, TyArg), ty_app(AbsRes, ArgRes)) :-
    subst(Ctx, TyAbs, AbsRes),
    subst(Ctx, TyArg, ArgRes).

% Haskell: Functor Deriving https://mail.haskell.org/pipermail/haskell-prime/2007-March/002137.html

%% Structural Template
struct_rule(class(ClassName, class_arg(_, Kind), _),
             rule(ClassName, opaque(Kind, _)) :- true
            ).

struct_rule(class(ClassName, class_arg(_, kind(*)), _),
            rule(ClassName, ty_ref(unit)) :- true
           ).

struct_rule(class(ClassName, class_arg(_, kind(*)), _),
            rule(ClassName, ty_ref(void)) :- true
           ).

struct_rule(class(ClassName, class_arg(_, kind(*)), _),
             rule(ClassName, ty_app(ty_app(ty_ref(prod), A), B)) :-
                 (
                     rule(ClassName, A),
                     rule(ClassName, B),
                     kind(ty_app(ty_app(ty_ref(prod), A), B), kind(*))
                 )
            ).

struct_rule(class(ClassName, class_arg(_, kind(*)), _),
             rule(ClassName, ty_app(ty_app(ty_ref(either), A), B)) :-
                 (
                     rule(ClassName, A),
                     rule(ClassName, B),
                     kind(ty_app(ty_app(ty_ref(either), A), B), kind(*))
                 )
            ).

struct_rule(class(ClassName, class_arg(_, kind(arr(KL, KR))), _),
            rule(ClassName, ty_app(TL, TR)) :-
                (
                    rule(ClassName, TL),
                    kind(ty_app(TL, TR), kind(arr(KL, KR)))
                )
           ).

struct_rule(class(ClassName, class_arg(_, Kind), _),
          (rule(ClassName, ty_abs(A, B)) :-
               (
                   kind(ty_abs(A, B), Kind),
                   rule(ClassName, B)
               )
          )
         ).

user_rule(class(ClassName, _, _),
          rule(ClassName, ty_ref(RefName)) :-
              (
                  ty_def(RefName, Ty),
                  rule(ClassName, Ty)
              )
         ).

user_rule(class(ClassName, class_arg(_, Kind), _),
           (rule(ClassName, ty_app(F, A)) :-
                (
                    kind(ty_app(F, A), Kind),
                    %% TODO(bladyjoker): Ask gnumonik@ if normalizing a Ty is what's really needed here. Imo, it is. Perhaps apply the rules on already normalized expressions?
                    apply(F, A, Res),
                    rule(ClassName, Res)
                )
           )
          ).

class_def(eq, class_arg(a, kind(*)), []).
class_def(ord, class_arg(a, kind(*)), [eq(a)]).
class_def(json, class_arg(a, kind(*)), []).
class_def(functor, class_arg(a, kind(arr(*, *))), []).


derive(Tys, CName, StructRules, UserRules) :-
    class_def(CName, CArg, CSups),
    findall(StructRule,
            (
                struct_rule(
                    class(CName, CArg, CSups),
                    (rule(CName, Ty) :- RuleBody)
                ),
                StructRule = (rule(CName, Ty) :- RuleBody)
            ),
            StructRules
           ),
    findall(UserRule,
            (
                member(Ty, Tys),
                user_rule(
                    class(CName, CArg, CSups),
                    (rule(CName, Ty) :- RuleBody)
                ),
                UserRule = (rule(CName, Ty) :- RuleBody)
            ),
            UserRules
           ).

eval_rules(StructRules, UserRules) :-
    append(StructRules, UserRules, Rules),
    member((RuleHead :- RuleBody), UserRules),
    print_message(informational, trying(RuleHead)),
    (
        eval_rule(Rules, [RuleHead], RuleBody) -> print_message(informational, rule_ok((RuleHead)));
        (
            print_message(error, rule_failed(RuleHead)),
            fail
        )
    ).

eval_rule(_, Trace, true) :-
    print_message(informational, rules_reached_true(Trace)).

eval_rule(Rules, Trace, (RL,RR)) :-
    print_message(informational, trying(RL)),
    eval_rule(Rules, Trace, RL),
    print_message(informational, rule_ok(RL)),

    print_message(informational, trying(RR)),
    eval_rule(Rules, Trace, RR),
    print_message(informational, rule_ok(RR)).

eval_rule(Rules, Trace, rule(CName, Ty)) :-
    first(rule(CName, Ty), Trace) -> true;
    (
        first((rule(CName, Ty) :- RuleBody), Rules) -> eval_rule(Rules, [rule(CName, Ty)|Trace], RuleBody);
        (
            print_message(error, missing_rule(rule(CName, Ty), Trace)),
            fail
        )
    ).

eval_rule(_, Trace, apply(F, A, Res)) :-
    apply(F, A, Res) -> print_message(info, applyx(F, A, Res)),true;
    (
        print_message(error, application_failed(Trace, F, A)),
        fail
    ).

eval_rule(_, _, kind(Ty, kind(Kind))) :-
    ty_kind(Ty, Kind) -> true;
    (
        (
            ty_kind(Ty, Kind_) -> print_message(error, wrong_kind(Ty, got(Kind_), wanted(Kind)));
            print_message(error, invalid_kind(Ty))
        ),
        fail
    ).

eval_rule(_, Trace, ty_def(RefName, Ty)) :-
    ty_def(RefName, Ty).

:- multifile prolog:message//1.

prolog:message(wrong_kind(Ty, got(Got), wanted(Want))) --> [ '~w is of kind ~w but wanted kind ~w'-[Ty, Got, Want]].
prolog:message(application_failed(_, F, A)) --> [ 'Application of ~w to ~w failed'-[F, A]].
prolog:message(trying(rule(ClassName, RuleHead))) --> [ 'Trying rule ~w ~w'-[ClassName, RuleHead]].
prolog:message(rule_ok(rule(ClassName, RuleHead))) --> [ 'Done with rule ~w ~w'-[ClassName, RuleHead]].
prolog:message(missing_rule(rule(ClassName, RuleHead), _)) --> [ 'Missing rule ~w ~w'-[ClassName, RuleHead]].
prolog:message(rule_failed(rule(ClassName, RuleHead))) --> [ 'Failed rule ~w ~w'-[ClassName, RuleHead]].

:- begin_tests(class_check).

test("should_succeed(Either Int8 Int8 :: *)", [ ]) :-
    apply(ty_ref(either), ty_ref(int8), R),
    apply(R, ty_ref(int8), R_),
    ty_kind(R_, K),
    K =@= (*).

test("should_succeed((\a -> a) bytes == bytes :: *)", [
     ]) :-
    apply(ty_abs(a-(*), ty_var(a)), ty_ref(bytes), R),
    ty_kind(R, K),
    R =@= ty_ref(bytes),
    K =@= (*).

test("should_fail(bytes int)", [
         fail
     ]) :-
    apply(ty_ref(bytes), ty_ref(int), _).

test("should_fail( (\a :: * -> (\a :: * -> a) a) int8 == (\a :: * -> a) int8 )", [
         fail
     ]) :-
    apply(ty_abs(a-(*), ty_app(ty_abs(a-(*), ty_var(a)), ty_var(a))), ty_ref(int8), Res),
    Res = ty_app(ty_abs(a-(*), ty_var(a)), ty_ref(int8)),
    apply(Res, ty_red(string), Res_).

test("should_succeed(derive_eq_of_int)", []) :-
    derive([ty_ref(int8)],eq,S,U), eval_rules(S, U).

test("should_succeed(derive_eq_of_maybe_int)", []) :-
    derive(
        [
            ty_ref(int8),
            ty_app(ty_ref(maybe), ty_ref(int8))
        ],
        eq,
        S,
        U),
    eval_rules(S, U).

test("should_succeed(derive_eq_of_maybe_a)", []) :-
    derive([ty_app(ty_ref(maybe), _A)], eq, S, U), eval_rules(S, U).

test("should_succeed(derive_functor_of_maybe)", []) :-
    derive([ty_ref(maybe)], functor,S,U),
    eval_rules(S, U).

test("should_succeed(derive_functor_of_either_int)", []) :-
    derive([ty_app(
                ty_ref(either),
                ty_ref(int8)
            )], functor, S, U), eval_rules(S, U).

test("should_fail(derive_functor_of_either)", [ fail ]) :-
    derive([ty_ref(either)], functor, S, U), eval_rules(S, U).

test("should_fail(derive_eq_of_foo)", [ fail ]) :-
    derive([ty_ref(foo)], eq, S, U), eval_rules(S, U).

:- end_tests(class_check).
