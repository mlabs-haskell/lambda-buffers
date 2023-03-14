'Prelude.Ord'('Prelude.Int8') :-
  'Prelude.Eq'('Prelude.Int8').
'Prelude.Ord'('Prelude.Bytes') :-
  'Prelude.Eq'('Prelude.Bytes').
'Prelude.Ord'('app'('Prelude.Maybe','|'(Va,'[]'))) :-
  'Prelude.Ord'(Va),'Prelude.Eq'('app'('Prelude.Maybe','|'(Va,'[]'))).
'Prelude.Ord'('app'('Prelude.Map','|'(Va,'|'(Vb,'[]')))) :-
  'Prelude.Ord'(Va)
  ,'Prelude.Ord'(Vb)
  ,'Prelude.Eq'('app'('Prelude.Map','|'(Va,'|'(Vb,'[]')))).
'Prelude.Ord'('app'('Prelude.List','|'(Va,'[]'))) :-
  'Prelude.Ord'(Va),'Prelude.Eq'('app'('Prelude.List','|'(Va,'[]'))).
'Prelude.Ord'('app'('Prelude.Either','|'(Va,'|'(Vb,'[]')))) :-
  'Prelude.Ord'(Va)
  ,'Prelude.Ord'(Vb)
  ,'Prelude.Eq'('app'('Prelude.Either','|'(Va,'|'(Vb,'[]')))).
'Prelude.Ord'('app'('Foo.Foo','|'(Va,'|'(Vb,'|'(Vc,'[]'))))) :-
  'Prelude.Ord'('sum'('|'('ctor'('MkFoo'
                                ,'tuple'('|'('app'('Foo.Bar','|'(Va,'[]'))
                                            ,'[]')))
                         ,'[]')))
  ,'Prelude.Eq'('app'('Foo.Foo','|'(Va,'|'(Vb,'|'(Vc,'[]'))))).
'Prelude.Ord'('app'('Foo.Bar','|'(Va,'[]'))) :-
  'Prelude.Eq'('app'('Foo.Bar','|'(Va,'[]'))).
'Prelude.Ord'('var'(VX)).
'Prelude.Ord'('rec'('[]')).
'Prelude.Ord'('rec'('|'('field'(VFn,VFTy),VFs))) :-
  'Prelude.Ord'(VFTy),'Prelude.Ord'('rec'(VFs)).
'Prelude.Ord'('tuple'('[]')).
'Prelude.Ord'('tuple'('|'(VH,VT))) :-
  'Prelude.Ord'(VH),'Prelude.Ord'('tuple'(VT)).
'Prelude.Ord'('sum'('[]')).
'Prelude.Ord'('sum'('|'('ctor'(VC,VP),VT))) :-
  'Prelude.Ord'(VP),'Prelude.Ord'('sum'(VT)).
'Prelude.Eq'('Prelude.Int8').
'Prelude.Eq'('Prelude.Bytes').
'Prelude.Eq'('app'('Prelude.Maybe','|'(Va,'[]'))) :-
  'Prelude.Eq'(Va).
'Prelude.Eq'('app'('Prelude.Map','|'(Va,'|'(Vb,'[]')))) :-
  'Prelude.Eq'(Va),'Prelude.Eq'(Vb).
'Prelude.Eq'('app'('Prelude.List','|'(Va,'[]'))) :-
  'Prelude.Eq'(Va).
'Prelude.Eq'('app'('Prelude.Either','|'(Va,'|'(Vb,'[]')))) :-
  'Prelude.Eq'(Va),'Prelude.Eq'(Vb).
'Prelude.Eq'('app'('Foo.Foo','|'(Va,'|'(Vb,'|'(Vc,'[]'))))) :-
  'Prelude.Eq'('sum'('|'('ctor'('MkFoo'
                               ,'tuple'('|'('app'('Foo.Bar','|'(Va,'[]'))
                                           ,'[]')))
                        ,'[]'))).
'Prelude.Eq'('app'('Foo.Bar','|'(Va,'[]'))).
'Prelude.Eq'('var'(VX)).
'Prelude.Eq'('rec'('[]')).
'Prelude.Eq'('rec'('|'('field'(VFn,VFTy),VFs))) :-
  'Prelude.Eq'(VFTy),'Prelude.Eq'('rec'(VFs)).
'Prelude.Eq'('tuple'('[]')).
'Prelude.Eq'('tuple'('|'(VH,VT))) :-
  'Prelude.Eq'(VH),'Prelude.Eq'('tuple'(VT)).
'Prelude.Eq'('sum'('[]')).
'Prelude.Eq'('sum'('|'('ctor'(VC,VP),VT))) :-
  'Prelude.Eq'(VP),'Prelude.Eq'('sum'(VT)).
