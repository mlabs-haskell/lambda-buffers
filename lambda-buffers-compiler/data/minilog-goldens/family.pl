:- module('family',['parent'/2
                    ,'ancestor'/2
                    ,'grandparent'/2
                    ,'ggrandparent'/2
                    ,'ggrandparent2'/2]).

'parent'('zoran','nenad').


'parent'('zdravka','slavka').


'parent'('vlado','zoran').


'parent'('slavko','slavka').


'parent'('slavka','nenad').


'parent'('mitar','ljeposava').


'parent'('ljeposava','zoran').


'parent'('dusan','vlado').


'grandparent'(VGp,VGc) :-
  'parent'(VGp,VP),'parent'(VP,VGc).


'ggrandparent2'(VGgp,VGgc) :-
  'grandparent'(VGgp,VP),'parent'(VP,VGgc).


'ggrandparent'(VGgp,VGgc) :-
  'parent'(VGgp,VGp),'parent'(VGp,VP),'parent'(VP,VGgc).


'ancestor'(VAnc,VX) :-
  'parent'(VAnc,VX),'ancestor'(VX,VDec).


'ancestor'(VAnc,VDec) :-
  'parent'(VAnc,VDec).
