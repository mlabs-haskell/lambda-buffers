# Protocol Documentation
<a name="top"></a>

## Table of Contents

- [compiler.proto](#compiler-proto)
    - [ClassConstraint](#lambdabuffers-compiler-ClassConstraint)
    - [ClassDef](#lambdabuffers-compiler-ClassDef)
    - [ClassName](#lambdabuffers-compiler-ClassName)
    - [CompilerError](#lambdabuffers-compiler-CompilerError)
    - [CompilerInput](#lambdabuffers-compiler-CompilerInput)
    - [CompilerOutput](#lambdabuffers-compiler-CompilerOutput)
    - [CompilerResult](#lambdabuffers-compiler-CompilerResult)
    - [ConstrName](#lambdabuffers-compiler-ConstrName)
    - [Constraint](#lambdabuffers-compiler-Constraint)
    - [Derive](#lambdabuffers-compiler-Derive)
    - [FieldName](#lambdabuffers-compiler-FieldName)
    - [InstanceClause](#lambdabuffers-compiler-InstanceClause)
    - [InternalError](#lambdabuffers-compiler-InternalError)
    - [Kind](#lambdabuffers-compiler-Kind)
    - [Kind.KindArrow](#lambdabuffers-compiler-Kind-KindArrow)
    - [KindCheckError](#lambdabuffers-compiler-KindCheckError)
    - [KindCheckError.CyclicKindError](#lambdabuffers-compiler-KindCheckError-CyclicKindError)
    - [KindCheckError.InconsistentTypeError](#lambdabuffers-compiler-KindCheckError-InconsistentTypeError)
    - [KindCheckError.UnboundTyRefError](#lambdabuffers-compiler-KindCheckError-UnboundTyRefError)
    - [KindCheckError.UnboundTyVarError](#lambdabuffers-compiler-KindCheckError-UnboundTyVarError)
    - [KindCheckError.UnificationError](#lambdabuffers-compiler-KindCheckError-UnificationError)
    - [Module](#lambdabuffers-compiler-Module)
    - [ModuleName](#lambdabuffers-compiler-ModuleName)
    - [ModuleNamePart](#lambdabuffers-compiler-ModuleNamePart)
    - [NamingError](#lambdabuffers-compiler-NamingError)
    - [Opaque](#lambdabuffers-compiler-Opaque)
    - [Product](#lambdabuffers-compiler-Product)
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
    - [Record](#lambdabuffers-compiler-Record)
    - [Record.Field](#lambdabuffers-compiler-Record-Field)
    - [SourceInfo](#lambdabuffers-compiler-SourceInfo)
    - [SourcePosition](#lambdabuffers-compiler-SourcePosition)
    - [Sum](#lambdabuffers-compiler-Sum)
    - [Sum.Constructor](#lambdabuffers-compiler-Sum-Constructor)
    - [Ty](#lambdabuffers-compiler-Ty)
    - [TyAbs](#lambdabuffers-compiler-TyAbs)
    - [TyApp](#lambdabuffers-compiler-TyApp)
    - [TyArg](#lambdabuffers-compiler-TyArg)
    - [TyBody](#lambdabuffers-compiler-TyBody)
    - [TyClassCheckError](#lambdabuffers-compiler-TyClassCheckError)
    - [TyClassCheckError.DeriveOpaqueError](#lambdabuffers-compiler-TyClassCheckError-DeriveOpaqueError)
    - [TyClassCheckError.ImportNotFoundError](#lambdabuffers-compiler-TyClassCheckError-ImportNotFoundError)
    - [TyClassCheckError.MissingRuleError](#lambdabuffers-compiler-TyClassCheckError-MissingRuleError)
    - [TyClassCheckError.OverlappingRulesError](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError)
    - [TyClassCheckError.OverlappingRulesError.QHead](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError-QHead)
    - [TyClassCheckError.SuperclassCycleError](#lambdabuffers-compiler-TyClassCheckError-SuperclassCycleError)
    - [TyClassCheckError.UnboundClassRefError](#lambdabuffers-compiler-TyClassCheckError-UnboundClassRefError)
    - [TyClassRef](#lambdabuffers-compiler-TyClassRef)
    - [TyClassRef.Foreign](#lambdabuffers-compiler-TyClassRef-Foreign)
    - [TyClassRef.Local](#lambdabuffers-compiler-TyClassRef-Local)
    - [TyDef](#lambdabuffers-compiler-TyDef)
    - [TyName](#lambdabuffers-compiler-TyName)
    - [TyRef](#lambdabuffers-compiler-TyRef)
    - [TyRef.Foreign](#lambdabuffers-compiler-TyRef-Foreign)
    - [TyRef.Local](#lambdabuffers-compiler-TyRef-Local)
    - [TyVar](#lambdabuffers-compiler-TyVar)
    - [Tys](#lambdabuffers-compiler-Tys)
    - [VarName](#lambdabuffers-compiler-VarName)
  
    - [Kind.KindRef](#lambdabuffers-compiler-Kind-KindRef)
  
- [Scalar Value Types](#scalar-value-types)



<a name="compiler-proto"></a>
<p align="right"><a href="#top">Top</a></p>

## compiler.proto



<a name="lambdabuffers-compiler-ClassConstraint"></a>

### ClassConstraint
Class constraints

A special constraint type denoting the constraints that occur on the rhs of
class definitions. Only used to specify super class constraints in a
`ClassDef`.

Not to be confused with `Constraint` which denote type class rules.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_ref | [TyClassRef](#lambdabuffers-compiler-TyClassRef) |  | Type class reference. |
| args | [TyVar](#lambdabuffers-compiler-TyVar) | repeated | Type variables quantified over `ClassDef` arguments. |






<a name="lambdabuffers-compiler-ClassDef"></a>

### ClassDef
Type class definition

LambdaBuffers use type classes to talk about the various &#39;meanings&#39; or
&#39;semantics&#39; we want to associate with the types in LambdaBuffers schemata.

For instance, most types can have `Eq` semantics, meaning they can be
compared for equality. Other can have `Ord` semantics, meaning they can be
ordered.

Using type classes and instance declarations, much like in Haskell, users can
specify the &#39;meaning&#39; of each type they declare. For example, serialization
in LambdaBuffers is just another type class, it&#39;s treated the same as any
other type class. Concretely, if we wish to provide JSON serialization for
LambdaBuffers types, we declare such a type class and provide desired
semantic rules:

```lbf module Foo

class JSON a

sum Foo a b = Bar a | Baz b

derive JSON (Foo a b) ```

Note that for each type class introduced, the Codegen machinery must be
updated to support said type class. In other words, it doesn&#39;t come for free
and for each new type class, a Codegen support must be implemented for any
`InstanceClause` declared by the user. Once all the `InstanceClause`s have an
implementation provided, all the `Derive`d implementation come for free.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  | Type class name. |
| class_args | [TyArg](#lambdabuffers-compiler-TyArg) | repeated | Type class arguments. Class with no arguments is a trivial class. Compiler MAY report an error. TODO(bladyjoker): MultipleClassArgError. |
| supers | [ClassConstraint](#lambdabuffers-compiler-ClassConstraint) | repeated | Superclass constraints. |
| documentation | [string](#string) |  | Documentation elaborating on the type class. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-ClassName"></a>

### ClassName
Type class name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [A-Z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






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
| modules | [Module](#lambdabuffers-compiler-Module) | repeated | Modules to compile. Duplicate modules MUST be reported with `ProtoParseError.MultipleModuleError`. |






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






<a name="lambdabuffers-compiler-ConstrName"></a>

### ConstrName
Sum type constructor name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [A-Z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Constraint"></a>

### Constraint
Constraint term


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_ref | [TyClassRef](#lambdabuffers-compiler-TyClassRef) |  | Name of the type class. |
| args | [Ty](#lambdabuffers-compiler-Ty) | repeated | Constraint arguments. Constraint with no arguments is a trivial constraint. Compiler MAY report an error. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Derive"></a>

### Derive
Derive statement

Derive statements enable user to specify &#39;semantic&#39; rules for their types much
like `InstanceClause`s do. However, the Codegen will be able to derive an
implementation for any such constraint.

```lbf
module Prelude

class Eq a

sum Maybe a = Just a | Nothing

derive Eq (Maybe a)
```

The rule installed for the derive statement is:

```prolog
eq(maybe(A)) :- eq(just(A) | Nothing).
```

The rule relates the desired `Ty` term to its (lambda calculus)
&#39;evaluated&#39; form.

&gt; Currently, there&#39;s only support for deriving type class rules and
implementations for `Ty` terms of `Kind.KIND_REF_TYPE`. That means,
type classes like Ord and Eq...


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  | Constraint to derive. |






<a name="lambdabuffers-compiler-FieldName"></a>

### FieldName
Record type field name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [a-z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-InstanceClause"></a>

### InstanceClause
Instance clause

Instance clauses enable users to specify ad-hoc &#39;semantic&#39; rules for their
types. Each such instance must be supported explicitly in the Codegen by
providing runtime implementations.

This rule form is used when declaring &#39;opaque&#39; implementations on `Opaque`
types.

```lbf
module Prelude

class Eq a

opaque Maybe a

instance Eq a =&gt; Eq (Maybe a)
```

The rule installed for the clause is:

```prolog
eq(maybe(A)) :- eq(A).
```

The instance clause is verbatim added to the rule set.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| head | [Constraint](#lambdabuffers-compiler-Constraint) |  | Head of the clause that holds only when the `body` holds. Type variables introduced in the head of the rule become available in the scope of the body of the rule. |
| constraints | [Constraint](#lambdabuffers-compiler-Constraint) | repeated | Instance (rule) body, conjunction of constraints. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-InternalError"></a>

### InternalError
Errors internal to the implementation.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| msg | [string](#string) |  | Error message. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information if meaningful. |






<a name="lambdabuffers-compiler-Kind"></a>

### Kind
Kinds

A type of a type is called a &#39;kind&#39;.
In Lambda Buffers, all type terms, namely TyArg, TyVar, TyRef, TyApp and TyAbs,
are either of kind `Type` or `Type -&gt; Type` and `Type -&gt; Type -&gt; Type`
etc.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| kind_ref | [Kind.KindRef](#lambdabuffers-compiler-Kind-KindRef) |  |  |
| kind_arrow | [Kind.KindArrow](#lambdabuffers-compiler-Kind-KindArrow) |  |  |






<a name="lambdabuffers-compiler-Kind-KindArrow"></a>

### Kind.KindArrow
A kind arrow.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| left | [Kind](#lambdabuffers-compiler-Kind) |  |  |
| right | [Kind](#lambdabuffers-compiler-Kind) |  |  |






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
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  |  |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-InconsistentTypeError"></a>

### KindCheckError.InconsistentTypeError
The actual_kind differs from the expected_kind.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  |  |
| actual_kind | [Kind](#lambdabuffers-compiler-Kind) |  |  |
| expected_kind | [Kind](#lambdabuffers-compiler-Kind) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnboundTyRefError"></a>

### KindCheckError.UnboundTyRefError
Unbound type reference ty_ref detected in term ty_def.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  |  |
| ty_ref | [TyRef](#lambdabuffers-compiler-TyRef) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnboundTyVarError"></a>

### KindCheckError.UnboundTyVarError
Unbound variable ty_var detected in term ty_def.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  |  |
| ty_var | [TyVar](#lambdabuffers-compiler-TyVar) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnificationError"></a>

### KindCheckError.UnificationError
In ty_def an error has occurred when trying to unify kind ty_kind_lhs
with ty_kind_rhs.

FIXME(cstml): Add source of constraint to the error such that user can see
where the constraint was generated - therefore where the error precisely
is.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  |  |
| ty_kind_lhs | [Kind](#lambdabuffers-compiler-Kind) |  |  |
| ty_kind_rhs | [Kind](#lambdabuffers-compiler-Kind) |  |  |






<a name="lambdabuffers-compiler-Module"></a>

### Module
Module

A module encapsulates type, class and instance definitions.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module name. |
| type_defs | [TyDef](#lambdabuffers-compiler-TyDef) | repeated | Type definitions. Duplicate type definitions MUST be reported with `ProtoParseError.MultipleTyDefError`. |
| class_defs | [ClassDef](#lambdabuffers-compiler-ClassDef) | repeated | Type class definitions. Duplicate class definitions MUST be reported with `ProtoParseError.MultipleClassDefError`. |
| instances | [InstanceClause](#lambdabuffers-compiler-InstanceClause) | repeated | Type class instance clauses. |
| derives | [Derive](#lambdabuffers-compiler-Derive) | repeated | Type class derive statements. |
| imports | [ModuleName](#lambdabuffers-compiler-ModuleName) | repeated | Imported modules the Compiler consults when searching for type class rules. TODO(bladyjoker): Rename to ruleImports. Duplicate imports MUST be reported with `ProtoParseError.MultipleImportError`. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-ModuleName"></a>

### ModuleName
Module name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| parts | [ModuleNamePart](#lambdabuffers-compiler-ModuleNamePart) | repeated | Parts of the module name denoting a hierarchichal namespace. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-ModuleNamePart"></a>

### ModuleNamePart
Module name part


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [A-Z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-NamingError"></a>

### NamingError
Naming error message


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name_err | [ModuleNamePart](#lambdabuffers-compiler-ModuleNamePart) |  |  |
| ty_name_err | [TyName](#lambdabuffers-compiler-TyName) |  |  |
| var_name_err | [VarName](#lambdabuffers-compiler-VarName) |  |  |
| constr_name_err | [ConstrName](#lambdabuffers-compiler-ConstrName) |  |  |
| field_name_err | [FieldName](#lambdabuffers-compiler-FieldName) |  |  |
| class_name_err | [ClassName](#lambdabuffers-compiler-ClassName) |  |  |






<a name="lambdabuffers-compiler-Opaque"></a>

### Opaque
Opaque type.

A type that has an `Opaque` body represents a &#39;built-in&#39; or a &#39;primitive&#39; type
that&#39;s handled by the semantics &#39;under the hood&#39;. It&#39;s called &#39;opaque&#39; to denote
the fact that the Compiler has no knowledge of its structure, and relies that
the necessary knowledge is implemented elsewhere. The Codegen modules for any
target language have to be able to handle such types specifically and map to
existing value level representations and corresponding types.

Codegen modules would have to implement support for such defined types, for
example:
- In Python `Set a` would map to `set()` from the standard library,
- In Haskell `Set a` would map to `containers`.Data.Set.Set type.

Every `Opaque` type has to be considered deliberately for each language
environment targeted by Codegen modules.

TODO(bladyjoker): Consider attaching explicit Kind terms to Opaques.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Product"></a>

### Product
A product type term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| fields | [Ty](#lambdabuffers-compiler-Ty) | repeated | Fields in a products are types. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






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
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module in which the error was found. |
| class_defs | [ClassDef](#lambdabuffers-compiler-ClassDef) | repeated | Conflicting class definitions. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleConstructorError"></a>

### ProtoParseError.MultipleConstructorError
Multiple Sum Constructors with the same ConstrName were found in
ModuleName.TyDef.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module in which the error was found. |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  | Type definition in which the error was found. |
| constructors | [Sum.Constructor](#lambdabuffers-compiler-Sum-Constructor) | repeated | Conflicting constructors. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleFieldError"></a>

### ProtoParseError.MultipleFieldError
Multiple Record Fields with the same FieldName were found in
ModuleName.TyDef.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module in which the error was found. |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  | Type definition in which the error was found. |
| fields | [Record.Field](#lambdabuffers-compiler-Record-Field) | repeated | Conflicting record fields. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleImportError"></a>

### ProtoParseError.MultipleImportError



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module in which the error was found. |
| imports | [ModuleName](#lambdabuffers-compiler-ModuleName) | repeated | Conflicting module imports. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleModuleError"></a>

### ProtoParseError.MultipleModuleError
Multiple Modules with the same ModuleName were found.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| modules | [Module](#lambdabuffers-compiler-Module) | repeated | Conflicting type definitions. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleTyArgError"></a>

### ProtoParseError.MultipleTyArgError
Multiple TyArgs with the same ArgName were found in ModuleName.TyDef.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module in which the error was found. |
| ty_def | [TyDef](#lambdabuffers-compiler-TyDef) |  | Type definition in which the error was found. |
| ty_args | [TyArg](#lambdabuffers-compiler-TyArg) | repeated | Conflicting type abstraction arguments. |






<a name="lambdabuffers-compiler-ProtoParseError-MultipleTyDefError"></a>

### ProtoParseError.MultipleTyDefError
Multiple TyDefs with the same TyName were found in ModuleName.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module in which the error was found. |
| ty_defs | [TyDef](#lambdabuffers-compiler-TyDef) | repeated | Conflicting type definitions. |






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






<a name="lambdabuffers-compiler-Record"></a>

### Record
A record type term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| fields | [Record.Field](#lambdabuffers-compiler-Record-Field) | repeated | Record fields. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Record-Field"></a>

### Record.Field
Field in a record type.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| field_name | [FieldName](#lambdabuffers-compiler-FieldName) |  | Record field name. |
| field_ty | [Ty](#lambdabuffers-compiler-Ty) |  | Field type. |






<a name="lambdabuffers-compiler-SourceInfo"></a>

### SourceInfo
Frontend Source information

Frontends are advised to include *Source* information to denote how their
Source* content maps to the *Compiler Input*. It&#39;s essential when reporting
Compiler* errors back to the Frontend.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| file | [string](#string) |  | A filename denoting the Source file. |
| pos_from | [SourcePosition](#lambdabuffers-compiler-SourcePosition) |  | Starting position in Source. |
| pos_to | [SourcePosition](#lambdabuffers-compiler-SourcePosition) |  | End position in Source. |






<a name="lambdabuffers-compiler-SourcePosition"></a>

### SourcePosition
Position in Source


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| column | [int32](#int32) |  | Column index in the Source. |
| row | [int32](#int32) |  | Row index in the Source. |






<a name="lambdabuffers-compiler-Sum"></a>

### Sum
A sum type term.

A type defined as a Sum type is just like a Haskell algebraic data type and
represents a sum of products.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constructors | [Sum.Constructor](#lambdabuffers-compiler-Sum-Constructor) | repeated | Sum type constructors. Empty `constructors` means `void` and means that the type can&#39;t be constructed. Compiler MAY report an error. Duplicate constructors MUST be reported with `ProtoParseError.MultipleConstructorError`. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Sum-Constructor"></a>

### Sum.Constructor
Constructor of a Sum type is a Product type term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constr_name | [ConstrName](#lambdabuffers-compiler-ConstrName) |  | Constructor name. |
| product | [Product](#lambdabuffers-compiler-Product) |  | Product type term. |






<a name="lambdabuffers-compiler-Ty"></a>

### Ty
Type term

A type term that ocurrs in bodies of type definitions (message TyDef):

```lbf
sum Maybe a = Just a | Nothing

sum Either a b = Left a | Right b

sum SomeType a = Foo a (Maybe a) | Bar (Either (Maybe a) (SomeType a))
```

or in instance declarations:

```lbf
instance Eq (Maybe a)

instance Eq (SomeType Int)

instance (Eq (Maybe a), Eq (SomeType a)) Eq (Either (Maybe a) (SomeType a))
```

Check out [examples](examples/tys.textproto).


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_var | [TyVar](#lambdabuffers-compiler-TyVar) |  | A type variable. |
| ty_app | [TyApp](#lambdabuffers-compiler-TyApp) |  | A type application. |
| ty_ref | [TyRef](#lambdabuffers-compiler-TyRef) |  | A type reference. |






<a name="lambdabuffers-compiler-TyAbs"></a>

### TyAbs
Type abstraction

A type term that introduces type abstractions (ie. type functions). This
type term can only be introduced in the context of a
[type definition](@ref TyDef).


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_args | [TyArg](#lambdabuffers-compiler-TyArg) | repeated | List of type variables. No type arguments means `delay` or `const ty_body`, meaning `TyAbs [] ty_body = ty_body`. Duplicate type arguments MUST be reported with `ProtoParseError.MultipleTyArgError`. |
| ty_body | [TyBody](#lambdabuffers-compiler-TyBody) |  | Type body. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyApp"></a>

### TyApp
Type application

A type term that applies a type abstraction to a list of arguments.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_func | [Ty](#lambdabuffers-compiler-Ty) |  | Type function. TODO(bladyjoker): Rename to ty_abs? |
| ty_args | [Ty](#lambdabuffers-compiler-Ty) | repeated | Arguments to apply. No arguments to apply means `force`, meaning `TyApp ty_func [] = ty_func`` |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyArg"></a>

### TyArg
Type arguments

Arguments in type abstractions.

Type arguments and therefore type variables have kinds, the Compiler only
accepts `Type` kinded type arguments ans therefore type variables.

However, to allow for future evolution if ever necessary, we attach the Kind
term to type arguments, even though the Compiler will reject any TyArg that&#39;s
not of kind `Type`.

Note, this effectively means that lambda Buffers doesn&#39;t support higher-kinded
types (ie. HKT).


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| arg_name | [VarName](#lambdabuffers-compiler-VarName) |  | Argument name corresponds to variable names. |
| arg_kind | [Kind](#lambdabuffers-compiler-Kind) |  | Argument kind. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyBody"></a>

### TyBody
Type body.

Lambda Buffers type bodies type terms that can only be specified in the
`TyAbs` context. It&#39;s a built-in type term that can only occur enclosed
within a `TyAbs` term which introduces `TyVar`s in the scope of the term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| opaque | [Opaque](#lambdabuffers-compiler-Opaque) |  |  |
| sum | [Sum](#lambdabuffers-compiler-Sum) |  |  |
| product | [Product](#lambdabuffers-compiler-Product) |  |  |
| record | [Record](#lambdabuffers-compiler-Record) |  |  |






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
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |
| sub_constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-ImportNotFoundError"></a>

### TyClassCheckError.ImportNotFoundError
Import `missing` wasn&#39;t found in `module_name`


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| missing | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-MissingRuleError"></a>

### TyClassCheckError.MissingRuleError
In `module_name` while trying to solve `constraint` it wasn&#39;t possible to
find a rule (`Derive` or `InstanceClause`) for `sub_constraint`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |
| sub_constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError"></a>

### TyClassCheckError.OverlappingRulesError
In `module_name` while trying to solve `constraint` `overlaps` (`Derive`
or `InstanceClause`) were found that could be used to solve the
`sub_constraint`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |
| sub_constraint | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |
| overlaps | [TyClassCheckError.OverlappingRulesError.QHead](#lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError-QHead) | repeated |  |






<a name="lambdabuffers-compiler-TyClassCheckError-OverlappingRulesError-QHead"></a>

### TyClassCheckError.OverlappingRulesError.QHead
NOTE(bladyjoker): This should rather be oneof `Derive` and
`InstanceClause`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| head | [Constraint](#lambdabuffers-compiler-Constraint) |  |  |






<a name="lambdabuffers-compiler-TyClassCheckError-SuperclassCycleError"></a>

### TyClassCheckError.SuperclassCycleError
Superclass cycle `cycled_class_refs` was detected when checking a
class definition for `class_name` in module `module_name`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  |  |
| cycled_class_refs | [TyClassRef](#lambdabuffers-compiler-TyClassRef) | repeated |  |






<a name="lambdabuffers-compiler-TyClassCheckError-UnboundClassRefError"></a>

### TyClassCheckError.UnboundClassRefError
Unbound `class_ref` detected in `module_name`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| class_ref | [TyClassRef](#lambdabuffers-compiler-TyClassRef) |  |  |






<a name="lambdabuffers-compiler-TyClassRef"></a>

### TyClassRef
Type class references

It is necessary to know whether a type class is defined locally or in a
foreign module when referring to it in a constraint, this allows users (and
requires the frontend) to explicitly communicate that information.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| local_class_ref | [TyClassRef.Local](#lambdabuffers-compiler-TyClassRef-Local) |  |  |
| foreign_class_ref | [TyClassRef.Foreign](#lambdabuffers-compiler-TyClassRef-Foreign) |  |  |






<a name="lambdabuffers-compiler-TyClassRef-Foreign"></a>

### TyClassRef.Foreign
Foreign class reference.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  | Foreign module class name. |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Foreign module name. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyClassRef-Local"></a>

### TyClassRef.Local
Local type reference.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  | Local module class name. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyDef"></a>

### TyDef
Type definition

A type definition consists of a type name and its associated type term.

One way to look at it is that a type definition introduces a named &#39;type
abstraction&#39; in the module scope. Concretely, `Either` can be considered a type
lambda of kind `Type -&gt; Type -&gt; Type`.

In fact, type definitions are the only way to introduce such types.

Once introduced in the module scope, type definitions are referred to using
[TyRef](@ref TyRef) term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  | Type name. |
| ty_abs | [TyAbs](#lambdabuffers-compiler-TyAbs) |  | Type term. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyName"></a>

### TyName
Type name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [A-Z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyRef"></a>

### TyRef
Type reference

A type term that denotes a reference to a type available that&#39;s declared
locally or in foreign modules.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| local_ty_ref | [TyRef.Local](#lambdabuffers-compiler-TyRef-Local) |  |  |
| foreign_ty_ref | [TyRef.Foreign](#lambdabuffers-compiler-TyRef-Foreign) |  |  |






<a name="lambdabuffers-compiler-TyRef-Foreign"></a>

### TyRef.Foreign
Foreign type reference.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  | Foreign module type name. |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Foreign module name. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyRef-Local"></a>

### TyRef.Local
Local type reference.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  | Local module type name. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyVar"></a>

### TyVar
Type variable


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_name | [VarName](#lambdabuffers-compiler-VarName) |  | Variable name. |






<a name="lambdabuffers-compiler-Tys"></a>

### Tys
A list of type terms useful for debugging


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ties | [Ty](#lambdabuffers-compiler-Ty) | repeated |  |






<a name="lambdabuffers-compiler-VarName"></a>

### VarName
Type variable name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [a-z]&#43; |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |





 


<a name="lambdabuffers-compiler-Kind-KindRef"></a>

### Kind.KindRef
A built-in kind.

| Name | Number | Description |
| ---- | ------ | ----------- |
| KIND_REF_UNSPECIFIED | 0 | Unspecified kind SHOULD be inferred by the Compiler. |
| KIND_REF_TYPE | 1 | A `Type` kind (also know as `*` in Haskell) built-in. |


 

 

 



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

<!-- markdownlint-disable-file -->
