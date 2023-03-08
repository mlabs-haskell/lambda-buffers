module Test.Utils.ClassDef (classDef'Eq, classDef'Ord, classInstance'IntEq, classInstance'OrdEq, classInstance'OrdEqFailing) where

import LambdaBuffers.Compiler.ProtoCompat qualified as PC
import Test.Utils.Constructors (_ClassDef, _Constraint, _InstanceClause, _InstanceClause', _LocalClassRef, _TyRefILocal, _TyVarI, _Type)

classDef'Eq :: PC.ClassDef
classDef'Eq = _ClassDef "Eq" ("a", _Type) mempty

classDef'Ord :: PC.ClassDef
classDef'Ord = _ClassDef "Ord" ("a", _Type) [_Constraint (_LocalClassRef "Eq") (_TyVarI "a")]

classInstance'IntEq :: PC.InstanceClause
classInstance'IntEq = _InstanceClause "Eq" (_TyRefILocal "Int")

classInstance'OrdEq :: PC.InstanceClause
classInstance'OrdEq = _InstanceClause' "Eq" (_TyVarI "a") [_Constraint (_LocalClassRef "Ord") (_TyVarI "a")]

classInstance'OrdEqFailing :: PC.InstanceClause
classInstance'OrdEqFailing = _InstanceClause' "Eq" (_TyVarI "a") [_Constraint (_LocalClassRef "Ord") (_TyVarI "b")]
