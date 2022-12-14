:- module(lb_checker, [
              ty_def/3,
              lb_error/1,
              lb_errors/1,
              check_ty_arg/2, check_ty_body/2, check_ty_cons/2, check_product/3, check_product_fqty/1, check_fqty/1,
              fqty_kind_arity/2, ty_arg/3,
              collect_check_product_fqtys/3,
              ty_to_fqty/3
          ]).

:- use_module(library(chr)).
:- use_module(library(clpfd)).

:- chr_constraint ty_def/3.
:- chr_constraint check_ty_arg/2, check_ty_body/2, check_ty_cons/2, check_product/3, check_product_fqty/1, check_fqty/1.
:- chr_constraint fqty_kind_arity/2, ty_arg/3.
:- chr_constraint lb_error/1, lb_errors/1.

:- chr_option(debug, on).

ty_def(TyName, TyArgs, _) ==> findall(check_ty_arg(TyName, TyArg), member(TyArg, TyArgs), Checks), writeln(Checks), maplist(call, Checks).
ty_def(TyName, _, TyBody) ==> check_ty_body(TyName, TyBody).

check_ty_arg(TyName, ty_arg(TyArgName, ty_kind(arity(0))))  <=> ty_arg(TyName, TyArgName, ty_kind(arity(0))).
check_ty_arg(TyName, ty_arg(TyArgName, ty_kind(arity(Ar)))) <=> lb_error(ty_var_nonzero_arity(TyName, TyArgName, got(Ar), expected(0))).
ty_arg(TyName, TyArgName, _) \ ty_arg(TyName, TyArgName, _) <=> lb_error(ty_arg_duplicate(TyName, TyArgName)).

insert_body_checks @ check_ty_body(_, ty_opaque)             <=> true.
insert_body_checks @ check_ty_body(TyName, ty_body(TyConss)) <=> findall(check_ty_cons(TyName, TyCons), member(TyCons, TyConss), Checks), maplist(call, Checks).

ty_cons_duplicate_name @ check_ty_cons(TyName, ty_cons(TyConsName, _)), check_ty_cons(TyName, ty_cons(TyConsName, _)) <=> lb_error(ty_cons_duplicate_name(TyName, TyConsName)).

product_checks @ check_ty_cons(TyName, ty_cons(TyConsName, TyProduct)) ==> check_product(TyName, TyConsName, TyProduct).
product_checks @ check_product(TyName, TyConsName, Tys)                <=> collect_check_product_fqtys(ctx(TyName, TyConsName), Tys, Checks), maplist(call, Checks).
product_checks @ check_product_fqty(FqTy)                              ==> check_fqty(FqTy).

product_ty_arity_rule @ fqty_kind_arity(FqTy, 0) \ check_product_fqty(FqTy)  <=> true.
product_ty_arity_rule @ fqty_kind_arity(FqTy, Ar) \ check_product_fqty(FqTy) <=> fqty(Ctx, _) = FqTy | lb_error(ty_product_nonzero_ty_arity(Ctx, got(Ar), expected(0))).
product_ty_arity_rule @ check_product_fqty(_) <=> true.

ty_var @ ty_arg(TyName, TyVarName, ty_kind(arity(Ar))) \ check_fqty(fqty(ctx(TyName, TyConsName), ty_var(TyVarName))) <=> fqty_kind_arity(fqty(ctx(TyName, TyConsName), ty_var(TyVarName)), Ar).
ty_var @ ty_def(TyName, TyArgs, _) \ check_fqty(fqty(ctx(TyName, TyConsName), ty_var(TyVarName))) <=> ty_args_names(TyArgs, TyArgsNames)
                                                                                  | lb_error(ty_var_not_in_scope(ctx(TyName, TyConsName), expected(TyArgsNames), got(TyVarName))).

ty_ref @ ty_def(RefTyName, RefTyArgs, _) \ check_fqty(fqty(Ctx, ty_ref(RefTyName))) <=> length(RefTyArgs, Ar)
                                                                                  | fqty_kind_arity(fqty(Ctx, ty_ref(RefTyName)), Ar).
ty_ref @ check_fqty(fqty(Ctx, ty_ref(RefTyName))) <=> lb_error(ty_ref_not_found(Ctx, got(RefTyName))).

ty_app @ check_fqty(fqty(Ctx, ty_app(TyAppFun, TyAppArgs))) ==> findall(fqty(Ctx, X), (member(X, TyAppArgs)), FqTyAppArgs) | check_fqty(fqty(Ctx, TyAppFun)), maplist(call(check_fqty), FqTyAppArgs).

ty_app @ fqty_kind_arity(fqty(Ctx, TyAppFun), Ar) \ check_fqty(fqty(Ctx, ty_app(TyAppFun, TyAppArgs))) <=> length(TyAppArgs, NArgs), Ar_ is Ar - NArgs, Ar_ #>= 0
                                                                                         | fqty_kind_arity(fqty(Ctx, ty_app(TyAppFun, TyAppArgs)), Ar_).
ty_app @ fqty_kind_arity(fqty(Ctx, TyAppFun), Ar) \ check_fqty(fqty(Ctx, ty_app(TyAppFun, TyAppArgs))) <=> length(TyAppArgs, NArgs)
                                                                                         | lb_error(ty_app_too_many_args(Ctx, got(NArgs), expected(max(Ar)))).


lb_error(Err) <=> lb_errors([Err]).
lb_errors(ErrsL), lb_errors(ErrsR) <=> append(ErrsL, ErrsR, Errs), lb_errors(Errs).

:- chr_constraint check_xs/1, check_xs_/1, check_x/1.

check_xs(Xs) ==> check_xs_(Xs).
check_xs_([]) <=> true.
check_xs_([X|Xs]) <=> check_x(X); check_xs_(Xs).

check_x(X) ==> writeln(seen(X)).

% Regular helper predicates
collect_check_product_fqtys(Ctx, Tys, Checks) :-
    findall(
        check_product_fqty(FqTy),
        (
            member(Ty, Tys),
            ty_to_fqty(Ctx, Ty, FqTy)
        ),
        Checks
    ).

ty_to_fqty(Ctx, ty_var(TyVarName), fqty(Ctx, ty_var(TyVarName))).
ty_to_fqty(Ctx, ty_ref(RefTyName), fqty(Ctx, ty_ref(RefTyName))).
ty_to_fqty(Ctx, ty_app(TyF, TyArgs), fqty(Ctx, ty_app(TyF, TyArgs))).

ty_args_names(TyArgs, TyArgsNames) :-
    setof(
        TyArgName,
        TyKind^(
            member(ty_arg(TyArgName, TyKind), TyArgs)
        ),
        TyArgsNames
    ).

:- begin_tests(lb_checker).
:- use_module(library(chr)).

findall_errors(Es) :-
    findall(
        Err,
        (
            find_chr_constraint(lb_errors(Errs)),
            member(Err, Errs)
        ),
        Es).

test(should_succeed) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Maybe"), [ty_var("a")])])])),
    chr_show_store(lb_checker),
    findall_errors([]).

test(should_fail_ty_var_not_in_scope) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Maybe"), [ty_var("c")])])])),
    chr_show_store(lb_checker),
    findall_errors([ty_var_not_in_scope(ctx("F","FA"),expected(["a"]),got("c"))]).

test(should_fail_ty_app_too_many_args) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Maybe"), [ty_var("a"), ty_var("a")])])])),
    chr_show_store(lb_checker),
    findall_errors([ty_app_too_many_args(ctx("F","FA"),got(2),expected(max(1)))]).

test(should_fail_ty_product_nonzero_ty_arity) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Maybe"), [])])])),
    chr_show_store(lb_checker),
    findall_errors([ty_product_nonzero_ty_arity(ctx("F","FA"),got(1),expected(0))]).

test(should_fail_ty_ref_not_found) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Naybe"), [ty_var("a")])])])),
    chr_show_store(lb_checker),
    findall_errors([ty_ref_not_found(ctx("F","FA"),got("Naybe"))]).

test(should_fail_ty_cons_duplicate_name) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", []), ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Maybe"), [ty_var("a")])]), ty_cons("FA", [])])),
    chr_show_store(lb_checker),
    findall_errors([ty_cons_duplicate_name("F", "FA")]).

test(should_fail_ty_arg_duplicate) :-
    writeln(""),
    ty_def("Maybe", [ty_arg("a", ty_kind(arity(0)))], ty_opaque),
    ty_def("F", [ty_arg("a", ty_kind(arity(0))), ty_arg("a", ty_kind(arity(0)))], ty_body([ty_cons("FA", [ty_var("a"), ty_app(ty_ref("Maybe"), [ty_var("a")])])])),
    chr_show_store(lb_checker),
    findall_errors([ty_arg_duplicate("F","a")]).

:- end_tests(lb_checker).
