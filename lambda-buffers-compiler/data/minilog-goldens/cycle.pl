:- module('cycle',['eq'/1]).

'eq'('int').


'eq'('rectype') :-
  'eq'('int'),'eq'('rectype').


'eq'('list'(VX)) :-
  'eq'(VX).


'eq'('foo'(VX)) :-
  'eq'('bar'(VX)).


'eq'('beep'(VX)) :-
  'eq'('beep'(VX)).


'eq'('baz'(VX)) :-
  'eq'('foo'(VX)).


'eq'('bar'(VX)) :-
  'eq'('baz'(VX)).
