:- module(kind_check, [ty_var/1, ty_kind/3, default_ctx/1]).

ty_var(VarName) :- member(VarName, ["a", "b", "c"]).

ty_kind(ctx(_, Refs), ty_ref(RefName), KtVar) :-
    member(RefName-KtVar, Refs).

ty_kind(ctx(Vars, _), ty_var(VarName), KtVar) :-
    ty_var(VarName),
    member(VarName-KtVar, Vars).

ty_kind(ctx(Vars, Refs), ty_abs(VarName, TyBody), KtAbs) :-
    KtVar = "*",
    append([VarName-KtVar], Vars, Vars_),
    ty_kind(ctx(Vars_, Refs), TyBody, KtBody),
    KtAbs = arr(KtVar, KtBody).

ty_kind(Ctx, ty_app(TyFun, TyArg), KtApp) :-
    ty_kind(Ctx, TyArg, KtArg),
    ty_kind(Ctx, TyFun, KtFun),
    KtFun = arr(KtArg, KtApp).


default_ctx(ctx(
                [
                ]
                , [
                    "Int"-"*",
                    "Maybe"-arr("*", "*"),
                    "Either"-arr("*", arr("*", "*"))
                ])).
