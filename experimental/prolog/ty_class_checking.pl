:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(clpfd)).
:- use_module(common_defs).
:- use_module(kind_check).

kind(_, kind(0)).


%% Structural rule generated for Eq
eq(opaque(kind(0), _)).

eq(ty_ref(unit)) :-
    kind(ty_ref(unit), kind(0)).

eq(ty_app(ty_app(ty_ref(prod), A), B)) :-
    eq(A),
    eq(B),
    kind(ty_app(ty_app(ty_ref(prod), A), B), kind(0)).

eq(ty_app(ty_app(ty_ref(either), A), B)) :-
    eq(A),
    eq(B),
    kind(ty_app(ty_app(ty_ref(either), A), B), kind(0)).

eq(ty_var(_)) :- writeln(no-flexible-instances), false.
eq(ty_abs(_, _)) :- writeln(wrong-kind), false.

% Users specified rules for Eq
eq(ty_ref(int8)) :-
    kind(ty_ref(int8), kind(0)).

eq(ty_ref(string)) :-
    kind(ty_ref(string), kind(0)).

eq(ty_app(ty_ref(maybe), A)) :-
    eq(A),
    ty_lam(ctx([]), ty_app(ty_ref(maybe), A), Res),
    eq(Res),
    kind(ty_app(ty_ref(maybe), A), kind(0)).

%% Structural rule generated for Eq
ord(opaque(kind(0), _)).

ord(ty_ref(unit)) :-
    kind(ty_ref(unit), kind(0)),
    eq(ty_ref(unit)).

ord(ty_app(ty_app(ty_ref(prod), A), B)) :-
    ord(A),
    ord(B),
    kind(ty_app(ty_app(ty_ref(prod), A), B), kind(0)),
    eq(ty_app(ty_app(ty_ref(prod), A), B)).

ord(ty_app(ty_app(ty_ref(either), A), B)) :-
    ord(A),
    ord(B),
    kind(ty_app(ty_app(ty_ref(either), A), B), kind(0)),
    eq(ty_app(ty_app(ty_ref(either), A), B)).

% Users specified rules for Eq
ord(ty_ref(int8)) :-
    kind(ty_ref(int8), kind(0)),
    eq(ty_ref(int8)).

ord(ty_ref(string)) :-
    kind(ty_ref(int8), kind(0)),
    eq(ty_ref(string)).

ord(ty_app(ty_ref(maybe), A)) :-
    ord(A),
    ty_lam(ctx([]), ty_app(ty_ref(maybe), A), Res),
    print(Res),
    ord(Res),
    kind(ty_app(ty_ref(maybe), A), kind(0)),
    eq(ty_app(ty_ref(maybe), A)).


%% Type DSL as Lambdas
ty_lam(Ctx, ty_ref(F), Lam) :-
    ty_def(F, FTy),
    ty_lam(Ctx, FTy, Lam).

ty_lam(_, opaque(kind(0), Cd), opaque(kind(0), Cd)).
ty_lam(Ctx, opaque(kind(K), Cd), Lam) :-
    K #> 0,
    K_ #= K - 1,
    ty_lam(Ctx, opaque(kind(K_), Cd), Lam_),
    Lam = [_, Lam_]>>true.

ty_lam(ctx(Vars), ty_abs(ArgName, TyBody), Lam)  :-
    Lam = [Arg, Res]>>ty_lam(ctx([ArgName-Arg|Vars]), TyBody, Res).

ty_lam(Ctx, ty_app(F, A), Lam)  :-
    ty_lam(Ctx, F, FLam),
    ty_lam(Ctx, A, ALam),
    call(FLam, ALam, Lam).

ty_lam(ctx(Vars), ty_var(VarName), Lam) :-
    first(VarName-Lam, Vars).

apply(F, A, Res) :-
    ty_lam(ctx([]), ty_app(F, A), Res).

%% Structural Template
struct_templ(class(ClassName, class_arg(ClassArgName, Kind), ClassSupers),
             rule(ClassName, opaque(Kind, _)) :- true
            ).

struct_templ(class(ClassName, class_arg(ClassArgName, Kind), ClassSupers),
             rule(ClassName, ty_app(ty_app(ty_ref(prod), A), B)) :-
                 (
                     rule(ClassName, A),
                     rule(ClassName, B),
                     kind(ty_app(ty_app(ty_ref(prod), A), B), kind(0))
                 )
            ).

struct_templ(class(ClassName, class_arg(ClassArgName, Kind), ClassSupers),
             rule(ClassName, ty_app(ty_app(ty_ref(either), A), B)) :-
                 (
                     rule(ClassName, A),
                     rule(ClassName, B),
                     kind(ty_app(ty_app(ty_ref(either), A), B), kind(0))
                 )
            ).

user_templ(class(ClassName, class_arg(ClassArgName, Kind), ClassSupers),
           rule(ClassName, ty_ref(RefName)) :-
               (
                   ty_def(RefName, Ty),
                   kind(Ty, Kind)
               )
          ).

user_templ(class(ClassName, class_arg(ClassArgName, Kind), ClassSupers),
           (rule(ClassName, ty_app(F, A)) :-
                (
                    rule(ClassName, A),
                    apply(F, A, Res),
                    rule(ClassName, Res),
                    kind(ty_app(F, A), Kind)
                )
           )
          ).

user_templ(class(ClassName, class_arg(ClassArgName, Kind), ClassSupers),
           (rule(ClassName, ty_abs(A, B)) :-
                (
                    kind(ty_abs(A, B), Kind),
                    error(todo)
                )
           )
          ).

class_def(eq, class_arg(a, kind(0)), []).
class_def(ord, class_arg(a, kind(0)), [eq(a)]).
class_def(json, class_arg(a, kind(0)), []).
class_def(functor, class_arg(a, kind(1)), []).


derive(Tys, CName, Rules) :-
    class_def(CName, CArg, CSups),
    findall(StructRule,
            (
                struct_templ(
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
                user_templ(
                    class(CName, CArg, CSups),
                    (rule(CName, Ty) :- RuleBody)
                ),
                UserRule = (rule(CName, Ty) :- RuleBody)
            ),
            UserRules
           ),
    writeln(UserRules),
    append(StructRules, UserRules, Rules).

eval_rules(Rules) :-
    member((RuleHead :- RuleBody), Rules),
    print_message(informational, working_on((RuleHead :- RuleBody))),
    eval_rule(Rules, RuleHead, RuleBody).

eval_rule(Rules, rule(CName, Ty), true) :-
    print_message(informational, rule_reached_true).

eval_rule(Rules, rule(CName, Ty), (RL,RR)) :-
    print_message(informational, trying(RL)),
    eval_rule(Rules, rule(CName, Ty), RL),
    print_message(informational, rule_ok(RL)),

    print_message(informational, trying(RR)),
    eval_rule(Rules, rule(CName, Ty), RR),
    print_message(informational, rule_ok(RR)).

eval_rule(Rules, rule(CName, Ty), rule(CName_, Ty_)) :-
    first((rule(CName_, Ty_) :- RuleBody), Rules) -> eval_rule(Rules, rule(CName_, Ty_), RuleBody);
    (
        print_message(error, missing_rule(rule(CName_, Ty_))),
        fail
    ).

eval_rule(Rules, rule(CName, Ty), apply(F, A, Res)) :-
    apply(F, A, Res) -> true;
    (
        print_message(error, application_failed(F, A, Res)),
        fail
    ).

eval_rule(Rules, rule(CName, _), kind(Ty, kind(Kind))) :-
    print_message(debug, kkk(ty_kind(Ty, Kind_))),
    ty_kind(Ty, Kind_),
    (
        Kind = Kind_ -> true;
        (
            print_message(error, wrong_kind(Ty, got(Kind_), wanted(Kind))),
            fail
        )
    ).

eval_rule(Rules, rule(CName, Ty), ty_def(RefName, Ty_)) :-
    ty_def(RefName, Ty_).
