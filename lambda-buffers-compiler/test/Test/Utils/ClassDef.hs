module Test.Utils.ClassDef (classDef'Eq, classDef'Ord) where

import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Test.Utils.Constructors (_ClassDef, _Constraint, _LocalClassRef, _TyVarI, _Type)

classDef'Eq :: PC.ClassDef
classDef'Eq = _ClassDef "Eq" ("a", _Type) mempty

classDef'Ord :: PC.ClassDef
classDef'Ord = _ClassDef "Ord" ("a", _Type) [_Constraint (_LocalClassRef "Eq") (_TyVarI "a")]
