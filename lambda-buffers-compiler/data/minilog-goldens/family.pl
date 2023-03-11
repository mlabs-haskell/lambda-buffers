'parent'('slavka','nenad').
'parent'('zoran','nenad').
'parent'('vlado','zoran').
'parent'('ljeposava','zoran').
'parent'('zdravka','slavka').
'parent'('slavko','slavka').
'parent'('mitar','ljeposava').
'parent'('dusan','vlado').
'ancestor'(VAnc,VDec) :-
  'parent'(VAnc,VDec).
'ancestor'(VAnc,VX) :-
  'parent'(VAnc,VX),'ancestor'(VX,VDec).
'grandparent'(VGp,VGc) :-
  'parent'(VGp,VP),'parent'(VP,VGc).
'ggrandparent'(VGgp,VGgc) :-
  'parent'(VGgp,VGp),'parent'(VGp,VP),'parent'(VP,VGgc).
'ggrandparent2'(VGgp,VGgc) :-
  'grandparent'(VGgp,VP),'parent'(VP,VGgc).