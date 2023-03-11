'eq'('int').
'eq'('bytes').
'eq'('maybe'(VA)) :-
  'eq'(VA).
'eq'('either'(VA,VB)) :-
  'eq'(VA),'eq'(VB).
'eq'('var'(VX)).