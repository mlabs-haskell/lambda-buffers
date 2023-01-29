:- module(common_defs, [ty_def/2, first/2]).

%% bottom types
ty_def(either,
       ty_abs(a-(*),
              ty_abs(b-(*),
                     ty_app(
                         ty_app(
                             opaque(
                                 kind(* -> * -> *),
                                 cardinality(
                                     ty_card(ty_var(a)) +
                                     ty_card(ty_var(b))
                                 )
                             ),
                             ty_var(a)
                         ),
                         ty_var(b)
                     )
                    )
             )
      ).

ty_def(prod,
       ty_abs(a-(*),
              ty_abs(b-(*),
                     ty_app(
                         ty_app(
                             opaque(
                                 kind(* -> * -> *),
                                 cardinality(
                                     ty_card(ty_var(a)) *
                                     ty_card(ty_var(b))
                                 )
                             ),
                             ty_var(a)
                         ),
                         ty_var(b)
                     )
                    )
             )
      ).
ty_def(void, opaque(kind(*), cardinality(k(0)))).
ty_def(unit, opaque(kind(*), cardinality(k(1)))).

ty_def(bool, opaque(kind(*), cardinality(k(2)))).

%% user opaques
ty_def(int8, opaque(kind(*), cardinality(k(256)))).
ty_def(string, opaque(kind(*), cardinality(k(sup)))).
ty_def(bytes, opaque(kind(*), cardinality(k(sup)))).

%% user defined
ty_def(maybe, ty_abs(a-(*),
                     ty_app(
                         ty_app(
                             ty_ref(either),
                             ty_ref(unit)
                         ),
                         ty_var(a)
                     )
                    )
      ).

% List a = Nil | Cons a (List a) = Either Unit (Prod a (List a))
ty_def(list, ty_abs(a-(*),
                    ty_app(
                        ty_app(
                            ty_ref(either),
                            ty_app(
                                ty_app(
                                    ty_ref(prod),
                                    ty_var(a)
                                ),
                                ty_app(
                                    ty_ref(list),
                                    ty_var(a)
                                )
                            )
                        ),
                        ty_ref(unit)
                    ))
      ).

ty_def(rec, ty_abs(a-(*), ty_app(ty_ref(rec), ty_var(a)))).

first(X, [H|_]) :-
    X = H.
first(X, [H|Xs]) :-
    X \= H,
    first(X, Xs).