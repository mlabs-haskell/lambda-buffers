:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, chars).

open_bracket --> "(", blanks.
closed_bracket --> blanks, ")".

ty(Ty) --> ty_var(Ty); ty_ref(Ty); ty_app(Ty).
ty(Ty) --> open_bracket, (ty_var(Ty); ty_ref(Ty); ty_app(Ty)), closed_bracket.

ty_(Ty) --> ty_var(Ty); ty_ref(Ty); ("(", ty_app(Ty), ")").

ty_var(Ty) --> [C], {member(C, ['a', 'b', 'c']), Ty = ty_var(C)}.

ty_ref(Ty) --> [C], {member(C, ['A', 'B', 'C']), Ty = ty_ref(C)}.


ty_app(Ty) --> (ty_var(TyF);ty_ref(TyF)), blanks, ty(TyA), {Ty = ty_app(TyF, TyA), TyA \= []}.
%% ty_app(Ty) --> ty_var(TyF), blanks, tys(TyA), {Ty = ty_app(TyF, TyA), TyA \= []}.
%% ty_app(Ty) --> ty_ref(TyF), blanks, tys(TyA), {Ty = ty_app(TyF, TyA), TyA \= []}.

tys([]) --> eos.
tys([Ty|Tys]) --> ty(Ty), blanks, tys(Tys).


