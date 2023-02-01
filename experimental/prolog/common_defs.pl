:- module(common_defs, [ty_def/2, first/2]).

%% bottom types
ty_def(either,
       ty_abs(a-(*),
              ty_abs(b-(*),
                     ty_app(
                         ty_app(
                             opaque(
                                 either,
                                 kind(arr(*, arr(*,*))),
                                 cardinality(ty_var(a) + ty_var(b))
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
                                 prod,
                                 kind(arr(*, arr(*,*))),
                                 cardinality(ty_var(a) * ty_var(b))
                             ),
                             ty_var(a)
                         ),
                         ty_var(b)
                     )
                    )
             )
      ).
ty_def(void, opaque(void, kind(*), cardinality(k(0)))).
ty_def(unit, opaque(unit, kind(*), cardinality(k(1)))).

%% user opaques
ty_def(bool, opaque(bool, kind(*), cardinality(k(2)))).
ty_def(int8, opaque(int8, kind(*), cardinality(k(256)))).
ty_def(string, opaque(string, kind(*), cardinality(k(sup)))).
ty_def(bytes, opaque(bytes, kind(*), cardinality(k(sup)))).

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

% data Rec a = X (Rec a)
ty_def(rec, ty_abs(a-(*),
                   ty_app(
                       ty_app(
                           ty_ref(prod),
                           ty_app(
                               ty_ref(rec),
                               ty_var(a)
                           )
                       ),
                       ty_ref(unit)
                   )
                  )
      ).

% data Foo = MkFoo (Maybe Int8)
ty_def(foo, ty_app(ty_ref(maybe),ty_ref(int8))).

% data RecFoo = RecFoo RecBar
ty_def(recfoo, ty_app(
                   ty_app(
                       ty_ref(prod),
                       ty_ref(recbar)
                   ),
                   ty_ref(unit)
               )
      ).

% data RecBar = RecBar RecFoo
ty_def(recbar, ty_app(
                   ty_app(
                       ty_ref(either),
                       ty_ref(recfoo)
                   ),
                   ty_ref(void)
               )
      ).

first(X, [H|Xs]) :-
    X = H -> true; first(X, Xs).
