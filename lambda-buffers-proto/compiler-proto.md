# Protocol Documentation
<a name="top"></a>

## Table of Contents

- [compiler.proto](#compiler-proto)
    - [ClassDef](#lambdabuffers-compiler-ClassDef)
    - [ClassName](#lambdabuffers-compiler-ClassName)
    - [CompilerError](#lambdabuffers-compiler-CompilerError)
    - [CompilerInput](#lambdabuffers-compiler-CompilerInput)
    - [CompilerOutput](#lambdabuffers-compiler-CompilerOutput)
    - [CompilerResult](#lambdabuffers-compiler-CompilerResult)
    - [ConstrName](#lambdabuffers-compiler-ConstrName)
    - [Constraint](#lambdabuffers-compiler-Constraint)
    - [FieldName](#lambdabuffers-compiler-FieldName)
    - [InstanceClause](#lambdabuffers-compiler-InstanceClause)
    - [InternalError](#lambdabuffers-compiler-InternalError)
    - [Kind](#lambdabuffers-compiler-Kind)
    - [Kind.KindArrow](#lambdabuffers-compiler-Kind-KindArrow)
    - [KindCheckError](#lambdabuffers-compiler-KindCheckError)
    - [KindCheckError.ImpossibleUnificationError](#lambdabuffers-compiler-KindCheckError-ImpossibleUnificationError)
    - [KindCheckError.InconsistentTypeError](#lambdabuffers-compiler-KindCheckError-InconsistentTypeError)
    - [KindCheckError.RecursiveKindError](#lambdabuffers-compiler-KindCheckError-RecursiveKindError)
    - [KindCheckError.UnboundTermError](#lambdabuffers-compiler-KindCheckError-UnboundTermError)
    - [Module](#lambdabuffers-compiler-Module)
    - [ModuleName](#lambdabuffers-compiler-ModuleName)
    - [ModuleNamePart](#lambdabuffers-compiler-ModuleNamePart)
    - [NamingError](#lambdabuffers-compiler-NamingError)
    - [Opaque](#lambdabuffers-compiler-Opaque)
    - [Product](#lambdabuffers-compiler-Product)
    - [Product.NTuple](#lambdabuffers-compiler-Product-NTuple)
    - [Product.Record](#lambdabuffers-compiler-Product-Record)
    - [Product.Record.Field](#lambdabuffers-compiler-Product-Record-Field)
    - [SourceInfo](#lambdabuffers-compiler-SourceInfo)
    - [SourcePosition](#lambdabuffers-compiler-SourcePosition)
    - [Sum](#lambdabuffers-compiler-Sum)
    - [Sum.Constructor](#lambdabuffers-compiler-Sum-Constructor)
    - [Ty](#lambdabuffers-compiler-Ty)
    - [TyAbs](#lambdabuffers-compiler-TyAbs)
    - [TyApp](#lambdabuffers-compiler-TyApp)
    - [TyArg](#lambdabuffers-compiler-TyArg)
    - [TyBody](#lambdabuffers-compiler-TyBody)
    - [TyDef](#lambdabuffers-compiler-TyDef)
    - [TyName](#lambdabuffers-compiler-TyName)
    - [TyRef](#lambdabuffers-compiler-TyRef)
    - [TyRef.Foreign](#lambdabuffers-compiler-TyRef-Foreign)
    - [TyRef.Local](#lambdabuffers-compiler-TyRef-Local)
    - [TyVar](#lambdabuffers-compiler-TyVar)
    - [Tys](#lambdabuffers-compiler-Tys)
    - [VarName](#lambdabuffers-compiler-VarName)
  
    - [Kind.KindRef](#lambdabuffers-compiler-Kind-KindRef)
    - [NamingError.NameType](#lambdabuffers-compiler-NamingError-NameType)
  
- [Scalar Value Types](#scalar-value-types)



<a name="compiler-proto"></a>
<p align="right"><a href="#top">Top</a></p>

## compiler.proto



<a name="lambdabuffers-compiler-ClassDef"></a>

### ClassDef
Type class definition

LambdaBuffers use type classes to talk about the various &#39;meanings&#39; or
&#39;semantics&#39; we want to associate with the types in LambdaBuffers schemata.

For instance, most types can have `Eq` semantics, meaning they can be compared
for equality. Other can have `Json` semantics, meaning they have some encoding
in the Json format.

Using type classes and instance declarations, much like in Haskell, users can
specify the &#39;meaning&#39; of each type they declare.

Note that for each type class introduced, the entire Codegen machinery must be
updated to support said type class. In other words, it doesn&#39;t come for free and
for each new type class, a Codegen support must be implemented for [opaque](@ref
Opaque) types
used and for generic structural rules to enable generic support for user derived
types.

TODO(bladyjoker): Cleanup and reformulate with Sean.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  | Type class name. |
| class_args | [TyArg](#lambdabuffers-compiler-TyArg) | repeated | Type class arguments. Currently the Compiler only accepts single parameter type class declarations. |
| supers | [Constraint](#lambdabuffers-compiler-Constraint) | repeated | Superclass constraints. |
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
Compiler Error can be extended with other classes of errors.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| kind_check_error | [KindCheckError](#lambdabuffers-compiler-KindCheckError) |  |  |
| internal_error | [InternalError](#lambdabuffers-compiler-InternalError) |  |  |






<a name="lambdabuffers-compiler-CompilerInput"></a>

### CompilerInput
Compiler Input

Compiler Input is a fully self contained list of modules, the entire
compilation closure needed by the Compiler to perform its task.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| modules | [Module](#lambdabuffers-compiler-Module) | repeated | Modules to compile. |






<a name="lambdabuffers-compiler-CompilerOutput"></a>

### CompilerOutput
Output of the Compiler.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| compilation_error | [CompilerError](#lambdabuffers-compiler-CompilerError) |  |  |
| compilation_result | [CompilerResult](#lambdabuffers-compiler-CompilerResult) |  |  |






<a name="lambdabuffers-compiler-CompilerResult"></a>

### CompilerResult
Compiler Result ~ a successful Compilation Output.

equivalent of unit.






<a name="lambdabuffers-compiler-ConstrName"></a>

### ConstrName
Sum type constructor name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [A-Z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Constraint"></a>

### Constraint
Constraint expression


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  | Name of the type class. |
| arguments | [Ty](#lambdabuffers-compiler-Ty) | repeated | Constraint arguments. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-FieldName"></a>

### FieldName
Record type field name


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | Name ::= [a-z]&#43;[A-Za-z0-9_]* |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-InstanceClause"></a>

### InstanceClause
Type class instances

Instance clauses enable users to specify &#39;semantic&#39; rules for their types.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  | Type class name. |
| heads | [Ty](#lambdabuffers-compiler-Ty) | repeated | Head of the instance clause. Currently, the Compiler only accepts single parameter type classes. |
| constraints | [Constraint](#lambdabuffers-compiler-Constraint) | repeated | Body of the rule, conjunction of constraints. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-InternalError"></a>

### InternalError
Internal errors.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| internal_error | [string](#string) |  |  |






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
| unbound_term_error | [KindCheckError.UnboundTermError](#lambdabuffers-compiler-KindCheckError-UnboundTermError) |  |  |
| unification_error | [KindCheckError.ImpossibleUnificationError](#lambdabuffers-compiler-KindCheckError-ImpossibleUnificationError) |  | FIXME(bladyjoker): Align naming. |
| recursive_subs_error | [KindCheckError.RecursiveKindError](#lambdabuffers-compiler-KindCheckError-RecursiveKindError) |  |  |
| inconsistent_type_error | [KindCheckError.InconsistentTypeError](#lambdabuffers-compiler-KindCheckError-InconsistentTypeError) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-ImpossibleUnificationError"></a>

### KindCheckError.ImpossibleUnificationError
In ty_name definition an error has occurred when trying to unify kind
ty_kind_1 with ty_kind_2.

FIXME(cstml): Add source of constraint to the error such that user can see
where the constraint was generated - therefore where the error precisely
is.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |
| ty_kind_1 | [Kind](#lambdabuffers-compiler-Kind) |  | FIXME(bladyjoker): Use _lhs. |
| ty_kind_2 | [Kind](#lambdabuffers-compiler-Kind) |  | FIXME(bladyjoker): Use _rhs. |






<a name="lambdabuffers-compiler-KindCheckError-InconsistentTypeError"></a>

### KindCheckError.InconsistentTypeError
Couldn&#39;t match expected_kind with actual_kind


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |
| inferred_kind | [Kind](#lambdabuffers-compiler-Kind) |  | FIXME(bladyjoker): Rename to actual. |
| defined_kind | [Kind](#lambdabuffers-compiler-Kind) |  | FIXME(bladyjoker): Rename to expected. |






<a name="lambdabuffers-compiler-KindCheckError-RecursiveKindError"></a>

### KindCheckError.RecursiveKindError
Inifinitely recursive term detected in definition ty_name.
FIXME(bladyjoker): Improve the reading of this, I don&#39;t know what it is.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |






<a name="lambdabuffers-compiler-KindCheckError-UnboundTermError"></a>

### KindCheckError.UnboundTermError
Error referring to an unbound term. This usually means that the term was
not defined.
FIXME(bladyjoker): Rename to UnboundTyVar, add reading.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |
| var_name | [VarName](#lambdabuffers-compiler-VarName) |  | FIXME(bladyjoker): Replace with TyVar. |






<a name="lambdabuffers-compiler-Module"></a>

### Module
Module

A module encapsulates type, class and instance definitions.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  | Module name. |
| type_defs | [TyDef](#lambdabuffers-compiler-TyDef) | repeated | Type definitions. |
| class_defs | [ClassDef](#lambdabuffers-compiler-ClassDef) | repeated | Type class definitions. |
| instances | [InstanceClause](#lambdabuffers-compiler-InstanceClause) | repeated | Type class instance clauses. |
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
| name_type | [NamingError.NameType](#lambdabuffers-compiler-NamingError-NameType) |  | Type of name. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Opaque"></a>

### Opaque
Opaque type body

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
Product

It&#39;s a built-in type term that exists enclosed within a [type abstraction](@ref
TyAbs) term which introduces [type variables](@ref TyVar) in the scope of the
expression.

It exists in two flavors, either a Record or a NTuple.

TODO(bladyjoker): Separate into Tuple and Record.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record | [Product.Record](#lambdabuffers-compiler-Product-Record) |  |  |
| ntuple | [Product.NTuple](#lambdabuffers-compiler-Product-NTuple) |  |  |






<a name="lambdabuffers-compiler-Product-NTuple"></a>

### Product.NTuple
A tuple type expression.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| fields | [Ty](#lambdabuffers-compiler-Ty) | repeated | Fields in a tuple are types. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Product-Record"></a>

### Product.Record
A record type expression.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| fields | [Product.Record.Field](#lambdabuffers-compiler-Product-Record-Field) | repeated | Record fields. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Product-Record-Field"></a>

### Product.Record.Field
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
Sum

It&#39;s a built-in type term that exists enclosed within a [type abstraction](@ref
TyAbs) term which introduces [type variables](@ref TyVar) in the scope of the
expression.

A type defined as a Sum type is just like a Haskell algebraic data type and
represents a sum of products.

It can essentially be expressed as `Either` type enriched with [constructor
name](@ref ConstrName) information.

```haskell

data Foo a b = Bar | Baz a | Bax b

-- corresponds to

type ConstrName = String
type Foo_ a b = Either
((), ConstrName)
(Either
(a, ConstrName)
(b, ConstrName)
)
```

TODO(bladyjoker): Cleanup.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constructors | [Sum.Constructor](#lambdabuffers-compiler-Sum-Constructor) | repeated | Sum type constructors. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Sum-Constructor"></a>

### Sum.Constructor
Constructor of a Sum type is a Product type term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constr_name | [ConstrName](#lambdabuffers-compiler-ConstrName) |  | Constructor name. |
| product | [Product](#lambdabuffers-compiler-Product) |  | TODO(bladyjoker): Replace with ConstructorBody that&#39;s either Tuple or Record. Product type term. |






<a name="lambdabuffers-compiler-Ty"></a>

### Ty
Type term

A type expression that ocurrs in bodies of type definitions (message TyDef):

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

A type expression that introduces type abstractions (ie. type functions). This
type term can only be introduced in the context of a
[type definition](@ref TyDef).


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_args | [TyArg](#lambdabuffers-compiler-TyArg) | repeated | List of arguments (possibly empty). |
| ty_body | [TyBody](#lambdabuffers-compiler-TyBody) |  | Type body. |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-TyApp"></a>

### TyApp
Type application

A type expression that applies a type abstraction to a list of arguments.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_func | [Ty](#lambdabuffers-compiler-Ty) |  | Type function. TODO(bladyjoker): Rename to ty_abs? |
| ty_args | [Ty](#lambdabuffers-compiler-Ty) | repeated | Arguments to apply. |
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
Type body

Lambda Buffers type bodies are enriched type terms that can only be specified in
[type abstraction terms](@ref TyAbs).

TODO: Add Tuple and Record type bodies.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| opaque | [Opaque](#lambdabuffers-compiler-Opaque) |  |  |
| sum | [Sum](#lambdabuffers-compiler-Sum) |  |  |






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

A type expression that denotes a reference to a type available that&#39;s declared
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
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  | Source information. |






<a name="lambdabuffers-compiler-Tys"></a>

### Tys
A list of type expressions useful for debugging


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



<a name="lambdabuffers-compiler-NamingError-NameType"></a>

### NamingError.NameType


| Name | Number | Description |
| ---- | ------ | ----------- |
| NAME_TYPE_UNSPECIFIED | 0 |  |
| NAME_TYPE_MODULE | 1 |  |
| NAME_TYPE_TYPE | 2 |  |
| NAME_TYPE_VAR | 3 |  |
| NAME_TYPE_CONSTR | 4 |  |
| NAME_TYPE_FIELD | 5 |  |
| NAME_TYPE_CLASS | 6 |  |


 

 

 



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
