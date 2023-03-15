:- module('greeks',['animal'/1,'human'/1]).

'human'('socrates').


'human'('plato').


'animal'(VX) :-
  'human'(VX).
