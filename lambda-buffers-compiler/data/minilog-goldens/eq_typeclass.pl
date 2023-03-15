:- module('eq_typeclass',['eq'/1]).

'eq'('int').


'eq'('bytes').


'eq'('var'(VX)).


'eq'('maybe'(VA)) :-
  'eq'(VA).


'eq'('either'(VA,VB)) :-
  'eq'(VA),'eq'(VB).
