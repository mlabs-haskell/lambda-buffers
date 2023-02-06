:-module(aux, [first/2]).

first(X, [H|Xs]) :-
    X = H -> true; first(X, Xs).
