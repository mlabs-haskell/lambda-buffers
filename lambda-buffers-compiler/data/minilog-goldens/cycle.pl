'eq'('list'(VX)) :-
  'eq'(VX).
'eq'('foo'(VX)) :-
  'eq'('bar'(VX)).
'eq'('bar'(VX)) :-
  'eq'('baz'(VX)).
'eq'('baz'(VX)) :-
  'eq'('foo'(VX)).
'eq'('beep'(VX)) :-
  'eq'('beep'(VX)).
'eq'('int').