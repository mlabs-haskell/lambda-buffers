# Protocol Documentation
<a name="top"></a>

## Table of Contents

- [compiler.proto](#compiler-proto)
    - [CompilerError](#lambdabuffers-compiler-CompilerError)
    - [CompilerInput](#lambdabuffers-compiler-CompilerInput)
    - [CompilerOutput](#lambdabuffers-compiler-CompilerOutput)
    - [CompilerResult](#lambdabuffers-compiler-CompilerResult)
    - [InternalError](#lambdabuffers-compiler-InternalError)
    - [KindCheckError](#lambdabuffers-compiler-KindCheckError)
    - [KindCheckError.CyclicKindError](#lambdabuffers-compiler-KindCheckError-CyclicKindError)
    - [KindCheckError.InconsistentTypeError](#lambdabuffers-compiler-KindCheckError-InconsistentTypeError)
    - [KindCheckError.UnboundTyRefError](#lambdabuffers-compiler-KindCheckError-UnboundTyRefError)
    - [KindCheckError.UnboundTyVarError](#lambdabuffers-compiler-KindCheckError-UnboundTyVarError)
    - [KindCheckError.UnificationError](#lambdabuffers-compiler-KindCheckError-UnificationError)
    - [NamingError](#lambdabuffers-compiler-NamingError)
    - [ProtoParseError](#lambdabuffers-compiler-ProtoParseError)
    - [ProtoParseError.MultipleClassDefError](#lambdabuffers-compiler-ProtoParseError-MultipleClassDefError)
    - [ProtoParseError.MultipleConstructorError](#lambdabuffers-compiler-ProtoParseError-MultipleConstructorError)
    - [ProtoParseError.MultipleFieldError](#lambdabuffers-compiler-ProtoParseError-MultipleFieldError)
    - [ProtoParseError.MultipleImportError](#lambdabuffers-compiler-ProtoParseError-MultipleImportError)
    - [ProtoParseError.MultipleModuleError](#lambdabuffers-compiler-ProtoParseError-MultipleModuleError)
    - [ProtoParseError.MultipleTyArgError](#lambdabuffers-compiler-ProtoParseError-MultipleTyArgError)
    - [ProtoParseError.MultipleTyDefError](#lambdabuffers-compiler-ProtoParseError-MultipleTyDefError)
    - [ProtoParseError.OneOfNotSetError](#lambdabuffers-compiler-ProtoParseError-OneOfNotSetError)
    - [ProtoParseError.UnknownEnumError](#lambdabuffers-compiler-ProtoParseError-UnknownEnumError)
    - [TyClassCheckError](#lambdabuffers-compiler-TyClassCheckError)
    - [TyClassCheckError.DeriveOpaqueError](#lambdabuffers-compiler-TyClassCheckError-DeriveOpaqueError)
    - [TyClassCheckError.ImportNotFoundError](#lambdabuffers-compiler-TyClassCheckError-ImportNotFoundError)
    - [TyClassCheckError.MissingRuleError](#lambdabuffers-compiler-TyClassCheckError-MissingRuleError)
    - [TyClassCheckError.OverlappingRulesError](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError)
    - [TyClassCheckError.OverlappingRulesError.QHead](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError-QHead)
    - [TyClassCheckError.SuperclassCycleError](#lambdabuffers-compiler-TyClassCheckError-SuperclassCycleError)
    - [TyClassCheckError.UnboundClassRefError](#lambdabuffers-compiler-TyClassCheckError-UnboundClassRefError)
  
- [Scalar Value Types](#scalar-value-types)



<a name="compiler-proto"></a>
<p align="right"><a href="#top">Top</a></p>

## compiler.proto



<a name="lambdabuffers-compiler-CompilerError"></a>

### CompilerError
Compiler Error


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| proto_parse_errors | [ProtoParseError](#lambdabuffers-compiler-ProtoParseError) | repeated | Errors occurred during proto parsing. |
| naming_errors | [NamingError](#lambdabuffers-compiler-NamingError) | repeated | Errors occurred during naming checking. |
| kind_check_errors | [KindCheckError](#lambdabuffers-compiler-KindCheckError) | repeated | Errors occurred during kind checking. |
| ty_class_check_errors | [TyClassCheckError](#lambdabuffers-compiler-TyClassCheckError) | repeated | Errors occurred during type class checking. |
| internal_errors | [InternalError](#lambdabuffers-compiler-InternalError) | repeated | Errors internal to the compiler implementation. |






<a name="lambdabuffers-compiler-CompilerInput"></a>

### CompilerInput
Compiler Input

Compiler Input is a fully self contained list of modules, the entire
compilation closure needed by the Compiler to perform its task.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| modules | [lambdabuffers.Module](#lambdabuffers-Module) | repeated | Modules to compile. Duplicate modules MUST be reported with `ProtoParseError.MultipleModuleError`. |






<a name="lambdabuffers-compiler-CompilerOutput"></a>

### CompilerOutput
Output of the Compiler.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| compiler_error | [CompilerError](#lambdabuffers-compiler-CompilerError) |  |  |
| compiler_result | [CompilerResult](#lambdabuffers-compiler-CompilerResult) |  |  |






<a name="lambdabuffers-compiler-CompilerResult"></a>

### CompilerResult
Compiler Result ~ a successful Compilation Output.






<a name="lambdabuffers-compiler-InternalError"></a>

### InternalError
Errors internal to the implementation.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| msg | [string](#string) |  | Error message. |
| source_info | [lambdabuffers.SourceInfo](#lambdabuffers-SourceInfo) |  | Source information if meaningful. |






<a name="lambdabuffers-compiler-KindCheckError"></a>

### KindCheckError
Kind checking errors.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| unbound_ty_ref_error | [KindCheckError.UnboundTyRefError](#lambdabuffers-compiler-KindCheckError-UnboundTyRefError) |  |  |
| unbound_ty_var_error | [KindCheckError.UnboundTyVarError](#lambdabuffers-compiler-KindCheckError-UnboundTyVarError) |  |  |
| unification_error | [KindCheckError.UnificationError](#lambdabuffers-compiler-KindCheckError-UnificationError) |  |  |
| cyclic_kind_error | [KindCheckError.CyclicKindError](#lambdabuffers-compiler-KindCheckError-CyclicKindError) |  |  |
| inconsistent_type_error | [KindCheckError.InconsistentTypeError](#lambdabuffers-compiler-KindCheckError-InconsistentTypeError) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-CyclicKindError"></a>

### KindCheckError.CyclicKindError
A cyclic kind was encountered. Infinite kinds like this are not acceptable,
and we do not support them. We could not construct infinite kind in ty_def.

As the implementation currently stands such an error is (most likely) not
representable - therefore not reachable. Such an error would usually occur
for a term like: λa. a a - in which case the inference would try to unify
two kinds of the form: m and m -&gt; n - because m appears in both terms -
the cyclic unification error would be thrown.

In the case of LambdaBuffers - such an error is not (currently) achievable
as the kind of the variable is given by the context - (i.e. λa : m . a a,
where m is a kind) therefore the unification would fail with Unification
Error. Nevertheless - future features might require it.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  |  |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-InconsistentTypeError"></a>

### KindCheckError.InconsistentTypeError
The actual_kind differs from the expected_kind.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  |  |
| actual_kind | [lambdabuffers.Kind](#lambdabuffers-Kind) |  |  |
| expected_kind | [lambdabuffers.Kind](#lambdabuffers-Kind) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnboundTyRefError"></a>

### KindCheckError.UnboundTyRefError
Unbound type reference ty_ref detected in term ty_def.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  |  |
| ty_ref | [lambdabuffers.TyRef](#lambdabuffers-TyRef) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnboundTyVarError"></a>

### KindCheckError.UnboundTyVarError
Unbound variable ty_var detected in term ty_def.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  |  |
| ty_var | [lambdabuffers.TyVar](#lambdabuffers-TyVar) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnificationError"></a>

### KindCheckError.UnificationError
In ty_def an error has occurred when trying to unify kind ty_kind_lhs
with ty_kind_rhs.

FIXME(cstml): Add source of constraint to the error such that user can see
where the constraint was generated - therefore where the error precisely
is.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  |  |
| ty_kind_lhs | [lambdabuffers.Kind](#lambdabuffers-Kind) |  |  |
| ty_kind_rhs | [lambdabuffers.Kind](#lambdabuffers-Kind) |  |  |






<a name="lambdabuffers-compiler-NamingError"></a>

### NamingError
Naming error message


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name_err | [lambdabuffers.ModuleNamePart](#lambdabuffers-ModuleNamePart) |  |  |
| ty_name_err | [lambdabuffers.TyName](#lambdabuffers-TyName) |  |  |
| var_name_err | [lambdabuffers.VarName](#lambdabuffers-VarName) |  |  |
| constr_name_err | [lambdabuffers.ConstrName](#lambdabuffers-ConstrName) |  |  |
| field_name_err | [lambdabuffers.FieldName](#lambdabuffers-FieldName) |  |  |
| class_name_err | [lambdabuffers.ClassName](#lambdabuffers-ClassName) |  |  |






<a name="lambdabuffers-compiler-ProtoParseError"></a>

### ProtoParseError
All errors that occur because of Google Protocol Buffer&#39;s inability to
enforce certain invariants.
Some of invariance:
- using Proto `map` restricts users to `string` keys which impacts
  API documentation, which is why `repeated` fields are used throughout,
- using Proto &#39;oneof&#39; means users have to check if such a field is
  set or report an error otherwise.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| multiple_module_error | [ProtoParseError.MultipleModuleError](#lambdabuffers-compiler-ProtoParseError-MultipleModuleError) |  |  |
| multiple_tydef_error | [ProtoParseError.MultipleTyDefError](#lambdabuffers-compiler-ProtoParseError-MultipleTyDefError) |  |  |
| multiple_classdef_error | [ProtoParseError.MultipleClassDefError](#lambdabuffers-compiler-ProtoParseError-MultipleClassDefError) |  |  |
| multiple_tyarg_error | [ProtoParseError.MultipleTyArgError](#lambdabuffers-compiler-ProtoParseError-MultipleTyArgError) |  |  |
| multiple_constructor_error | [ProtoParseError.MultipleConstructorError](#lambdabuffers-compiler-ProtoParseError-MultipleConstructorError) |  |  |
| multiple_field_error | [ProtoParseError.MultipleFieldError](#lambdabuffers-compiler-ProtoParseError-MultipleFieldError) |  |  |
| multiple_import_error | [ProtoParseError.MultipleImportError](#lambdabuffers-compiler-ProtoParseError-MultipleImportError) |  |  |
| one_of_not_set_error | [ProtoParseError.OneOfNotSetError](#lambdabuffers-compiler-ProtoParseError-OneOfNotSetError) |  |  |
| unknown_enum_error | [ProtoParseError.UnknownEnumError](#lambdabuffers-compiler-ProtoParseError-UnknownEnumError) |  |  |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleClassDefError"></a>

### ProtoParseError.MultipleClassDefError
Multiple ClassDefs with the same ClassName were found in ModuleName.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  | Module in which the error was found. |
| class_defs | [lambdabuffers.ClassDef](#lambdabuffers-ClassDef) | repeated | Conflicting class definitions. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleConstructorError"></a>

### ProtoParseError.MultipleConstructorError
Multiple Sum Constructors with the same ConstrName were found in
ModuleName.TyDef.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  | Module in which the error was found. |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  | Type definition in which the error was found. |
| constructors | [lambdabuffers.Sum.Constructor](#lambdabuffers-Sum-Constructor) | repeated | Conflicting constructors. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleFieldError"></a>

### ProtoParseError.MultipleFieldError
Multiple Record Fields with the same FieldName were found in
ModuleName.TyDef.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  | Module in which the error was found. |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  | Type definition in which the error was found. |
| fields | [lambdabuffers.Record.Field](#lambdabuffers-Record-Field) | repeated | Conflicting record fields. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleImportError"></a>

### ProtoParseError.MultipleImportError



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  | Module in which the error was found. |
| imports | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) | repeated | Conflicting module imports. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleModuleError"></a>

### ProtoParseError.MultipleModuleError
Multiple Modules with the same ModuleName were found.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| modules | [lambdabuffers.Module](#lambdabuffers-Module) | repeated | Conflicting type definitions. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleTyArgError"></a>

### ProtoParseError.MultipleTyArgError
Multiple TyArgs with the same ArgName were found in ModuleName.TyDef.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  | Module in which the error was found. |
| ty_def | [lambdabuffers.TyDef](#lambdabuffers-TyDef) |  | Type definition in which the error was found. |
| ty_args | [lambdabuffers.TyArg](#lambdabuffers-TyArg) | repeated | Conflicting type abstraction arguments. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleTyDefError"></a>

### ProtoParseError.MultipleTyDefError
Multiple TyDefs with the same TyName were found in ModuleName.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  | Module in which the error was found. |
| ty_defs | [lambdabuffers.TyDef](#lambdabuffers-TyDef) | repeated | Conflicting type definitions. |






<a name="lambdabuffers-compiler-ProtoParseError-OneOfNotSetError"></a>

### ProtoParseError.OneOfNotSetError
Proto `oneof` field is not set.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| message_name | [string](#string) |  | Proto message name in which the `oneof` field is not set. |
| field_name | [string](#string) |  | The `oneof` field that is not set. |






<a name="lambdabuffers-compiler-ProtoParseError-UnknownEnumError"></a>

### ProtoParseError.UnknownEnumError
Proto `enum` field is unknown.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| enum_name | [string](#string) |  | Proto `enum` name. |
| got_tag | [string](#string) |  | The unknown tag for the `enum`. |






<a name="lambdabuffers-compiler-TyClassCheckError"></a>

### TyClassCheckError
Type class checking errors.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| unbound_class_ref_err | [TyClassCheckError.UnboundClassRefError](#lambdabuffers-compiler-TyClassCheckError-UnboundClassRefError) |  |  |
| superclass_cycle_err | [TyClassCheckError.SuperclassCycleError](#lambdabuffers-compiler-TyClassCheckError-SuperclassCycleError) |  |  |
| import_not_found_err | [TyClassCheckError.ImportNotFoundError](#lambdabuffers-compiler-TyClassCheckError-ImportNotFoundError) |  |  |
| derive_opaque_err | [TyClassCheckError.DeriveOpaqueError](#lambdabuffers-compiler-TyClassCheckError-DeriveOpaqueError) |  |  |
| missing_rule_err | [TyClassCheckError.MissingRuleError](#lambdabuffers-compiler-TyClassCheckError-MissingRuleError) |  |  |
| overlapping_rules_err | [TyClassCheckError.OverlappingRulesError](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-DeriveOpaqueError"></a>

### TyClassCheckError.DeriveOpaqueError
In `module_name` it wasn&#39;t possible to solve `constraint` because a
`sub_constraint` has been derived on an `Opaque` type. `Opaque` type can
only have an `InstanceClause` declared for them.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| constraint | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |
| sub_constraint | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-ImportNotFoundError"></a>

### TyClassCheckError.ImportNotFoundError
Import `missing` wasn&#39;t found in `module_name`


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| missing | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-MissingRuleError"></a>

### TyClassCheckError.MissingRuleError
In `module_name` while trying to solve `constraint` it wasn&#39;t possible to
find a rule (`Derive` or `InstanceClause`) for `sub_constraint`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| constraint | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |
| sub_constraint | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError"></a>

### TyClassCheckError.OverlappingRulesError
In `module_name` while trying to solve `constraint` `overlaps` (`Derive`
or `InstanceClause`) were found that could be used to solve the
`sub_constraint`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| constraint | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |
| sub_constraint | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |
| overlaps | [TyClassCheckError.OverlappingRulesError.QHead](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError-QHead) | repeated |  |






<a name="lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError-QHead"></a>

### TyClassCheckError.OverlappingRulesError.QHead
NOTE(bladyjoker): This should rather be oneof `Derive` and
`InstanceClause`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| head | [lambdabuffers.Constraint](#lambdabuffers-Constraint) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-SuperclassCycleError"></a>

### TyClassCheckError.SuperclassCycleError
Superclass cycle `cycled_class_refs` was detected when checking a
class definition for `class_name` in module `module_name`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| class_name | [lambdabuffers.ClassName](#lambdabuffers-ClassName) |  |  |
| cycled_class_refs | [lambdabuffers.TyClassRef](#lambdabuffers-TyClassRef) | repeated |  |






<a name="lambdabuffers-compiler-TyClassCheckError-UnboundClassRefError"></a>

### TyClassCheckError.UnboundClassRefError
Unbound `class_ref` detected in `module_name`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [lambdabuffers.ModuleName](#lambdabuffers-ModuleName) |  |  |
| class_ref | [lambdabuffers.TyClassRef](#lambdabuffers-TyClassRef) |  |  |





 

 

 

 



## Scalar Value Types

| .proto Type | Notes | C++ | Java | Python | Go | C# | PHP | Ruby |
| ----------- | ----- | --- | ---- | ------ | -- | -- | --- | ---- |
| <a name="double" /> double |  | double | double | float | float64 | double | float | Float |
| <a name="float" /> float |  | float | float | float | float32 | float | float | Float |
| <a name="int32" /> int32 | Uses variable-length encoding. Inefficient for encoding negative numbers – if your field is likely to have negative values, use sint32 instead. | int32 | int | int | int32 | int | integer | Bignum or Fixnum (as required) |
| <a name="int64" /> int64 | Uses variable-length encoding. Inefficient for encoding negative numbers – if your field is likely to have negative values, use sint64 instead. | int64 | long | int/long | int64 | long | integer/string | Bignum |
| <a name="uint32" /> uint32 | Uses variable-length encoding. | uint32 | int | int/long | uint32 | uint | integer | Bignum or Fixnum (as required) |
| <a name="uint64" /> uint64 | Uses variable-length encoding. | uint64 | long | int/long | uint64 | ulong | integer/string | Bignum or Fixnum (as required) |
| <a name="sint32" /> sint32 | Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int32s. | int32 | int | int | int32 | int | integer | Bignum or Fixnum (as required) |
| <a name="sint64" /> sint64 | Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int64s. | int64 | long | int/long | int64 | long | integer/string | Bignum |
| <a name="fixed32" /> fixed32 | Always four bytes. More efficient than uint32 if values are often greater than 2^28. | uint32 | int | int | uint32 | uint | integer | Bignum or Fixnum (as required) |
| <a name="fixed64" /> fixed64 | Always eight bytes. More efficient than uint64 if values are often greater than 2^56. | uint64 | long | int/long | uint64 | ulong | integer/string | Bignum |
| <a name="sfixed32" /> sfixed32 | Always four bytes. | int32 | int | int | int32 | int | integer | Bignum or Fixnum (as required) |
| <a name="sfixed64" /> sfixed64 | Always eight bytes. | int64 | long | int/long | int64 | long | integer/string | Bignum |
| <a name="bool" /> bool |  | bool | boolean | boolean | bool | bool | boolean | TrueClass/FalseClass |
| <a name="string" /> string | A string must always contain UTF-8 encoded or 7-bit ASCII text. | string | String | str/unicode | string | string | string | String (UTF-8) |
| <a name="bytes" /> bytes | May contain any arbitrary sequence of bytes. | string | ByteString | str | []byte | ByteString | string | String (ASCII-8BIT) |

