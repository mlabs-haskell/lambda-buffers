# Protocol Documentation
<a name="top"></a>

## Table of Contents

- [compiler.proto](#compiler-proto)
    - [ClassDef](#lambdabuffers-compiler-ClassDef)
    - [ClassName](#lambdabuffers-compiler-ClassName)
    - [CompilerInput](#lambdabuffers-compiler-CompilerInput)
    - [ConstrName](#lambdabuffers-compiler-ConstrName)
    - [Constraint](#lambdabuffers-compiler-Constraint)
    - [FieldName](#lambdabuffers-compiler-FieldName)
    - [InstanceClause](#lambdabuffers-compiler-InstanceClause)
    - [Kind](#lambdabuffers-compiler-Kind)
    - [Kind.KindArrow](#lambdabuffers-compiler-Kind-KindArrow)
    - [Module](#lambdabuffers-compiler-Module)
    - [ModuleName](#lambdabuffers-compiler-ModuleName)
    - [ModuleNamePart](#lambdabuffers-compiler-ModuleNamePart)
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
  
- [Scalar Value Types](#scalar-value-types)



<a name="compiler-proto"></a>
<p align="right"><a href="#top">Top</a></p>

## compiler.proto
Run with: protoc --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
--haskell_out=proto_out compiler.proto


<a name="lambdabuffers-compiler-ClassDef"></a>

### ClassDef
Type class definitions

class (A a b, B c) &lt;= C a b c


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  |  |
| class_args | [TyArg](#lambdabuffers-compiler-TyArg) | repeated |  |
| supers | [Constraint](#lambdabuffers-compiler-Constraint) | repeated |  |
| documentation | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-ClassName"></a>

### ClassName
Regex [A-Z]&#43;[A-Za-z0-9_]*


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-CompilerInput"></a>

### CompilerInput
Compiler Input

Compiler Inputs is a fully self contained list of modules, the entire
compilation closure.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| modules | [Module](#lambdabuffers-compiler-Module) | repeated |  |






<a name="lambdabuffers-compiler-ConstrName"></a>

### ConstrName
Regex [A-Z]&#43;[A-Za-z0-9_]*


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Constraint"></a>

### Constraint



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  |  |
| arguments | [Ty](#lambdabuffers-compiler-Ty) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-FieldName"></a>

### FieldName
Regex [a-z]&#43;[A-Za-z0-9_]*


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-InstanceClause"></a>

### InstanceClause



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| class_name | [ClassName](#lambdabuffers-compiler-ClassName) |  |  |
| heads | [Ty](#lambdabuffers-compiler-Ty) | repeated |  |
| constraints | [Constraint](#lambdabuffers-compiler-Constraint) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Kind"></a>

### Kind
Kinds

Type kinds are quite simple in Lambda Buffers, all types (TyArg, TyVar, TyRef,
TyApp) are either of kind `Type` or `Type -&gt; Type` and `Type -&gt; Type -&gt; Type`
etc.

Because of that we can simply encode them using the notion of `arity` which
represents a number of type arguments that have to be applied to a type of `Type
-&gt; Type -&gt; .. -&gt; Type` to get a `Type` (ie. fully applied function, fully
saturated).


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| kind_ref | [Kind.KindRef](#lambdabuffers-compiler-Kind-KindRef) |  |  |
| kind_arrow | [Kind.KindArrow](#lambdabuffers-compiler-Kind-KindArrow) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Kind-KindArrow"></a>

### Kind.KindArrow



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| left | [Kind](#lambdabuffers-compiler-Kind) |  |  |
| right | [Kind](#lambdabuffers-compiler-Kind) |  |  |






<a name="lambdabuffers-compiler-Module"></a>

### Module
Module

A module encapsulates type, class and instance definitions.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |
| type_defs | [TyDef](#lambdabuffers-compiler-TyDef) | repeated |  |
| class_defs | [ClassDef](#lambdabuffers-compiler-ClassDef) | repeated |  |
| instances | [InstanceClause](#lambdabuffers-compiler-InstanceClause) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-ModuleName"></a>

### ModuleName
Regex (ModuleNamePart|.)&#43;


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| parts | [ModuleNamePart](#lambdabuffers-compiler-ModuleNamePart) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-ModuleNamePart"></a>

### ModuleNamePart
Regex [A-Z]&#43;[A-Za-z0-9_]*


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Opaque"></a>

### Opaque
Opaque type body

A type that has an `Opaque` body represents a &#39;builtin&#39; or a &#39;primitive&#39; type
that&#39;s handled by the semantics &#39;under the hood&#39;. It&#39;s called &#39;opaque&#39; to denote
the fact that the Compiler has no knowledge of its structure, and relies that
the necessary knowledge is implemented elsewhere. The Codegen modules for any
target language have to be able to handle such types specifically, for instance:

```haskell
opaque Int32
opaque Int64
opaque Set a
opaque Map k v
```

Codegen modules would have to implement support for such defined types, for
example:
- In Python `Set a` would map to `set()` from the standard library,
- In Haskell `Set a` would map to `containers`.Data.Set.Set type.

Every `Opaque` type has to be considered deliberately for each language
environment targeted by Codegen modules.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Product"></a>

### Product
Product

```haskell
data SomeType a b = A a b | B { fieldA :: a, fieldB :: b } | C
ttt     rrrrrrrrrrrrrrrrrrrrrrrrrrrr    e
```
- t - denotes the Product.NTuple
- r - denotes the Product.Record
- e - denotes the Product.Empty


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| record | [Product.Record](#lambdabuffers-compiler-Product-Record) |  |  |
| ntuple | [Product.NTuple](#lambdabuffers-compiler-Product-NTuple) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Product-NTuple"></a>

### Product.NTuple



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| fields | [Ty](#lambdabuffers-compiler-Ty) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Product-Record"></a>

### Product.Record



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| fields | [Product.Record.Field](#lambdabuffers-compiler-Product-Record-Field) | repeated |  |
| source_infog | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Product-Record-Field"></a>

### Product.Record.Field



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| field_name | [FieldName](#lambdabuffers-compiler-FieldName) |  |  |
| field_ty | [Ty](#lambdabuffers-compiler-Ty) |  |  |






<a name="lambdabuffers-compiler-SourceInfo"></a>

### SourceInfo



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| file | [string](#string) |  |  |
| pos_from | [SourcePosition](#lambdabuffers-compiler-SourcePosition) |  |  |
| pos_to | [SourcePosition](#lambdabuffers-compiler-SourcePosition) |  |  |






<a name="lambdabuffers-compiler-SourcePosition"></a>

### SourcePosition



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| column | [int32](#int32) |  |  |
| row | [int32](#int32) |  |  |






<a name="lambdabuffers-compiler-Sum"></a>

### Sum
Sum

A type defined as a Sum type is just like a Haskell ADT and represents a sum of
products, for example:

```haskell
data SomeType a b = A a b (Either a b) | B b a (Either b a)
c pppppppppppppppp   c pppppppppppppppp
```

- c - denotes a ConstrName
- p - denotes a Product

A `Sum.Constructor` term describes a type of the &#39;constructor&#39; function that
when fully applied yields the parent type. For instance, the constructor `A` is
a &#39;term function&#39; of type `A :: a -&gt; b -&gt; Either a b -&gt; SomeType a b`.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constructors | [Sum.Constructor](#lambdabuffers-compiler-Sum-Constructor) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Sum-Constructor"></a>

### Sum.Constructor



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| constr_name | [ConstrName](#lambdabuffers-compiler-ConstrName) |  |  |
| product | [Product](#lambdabuffers-compiler-Product) |  |  |






<a name="lambdabuffers-compiler-Ty"></a>

### Ty
Ty

A type expression that ocurrs in bodies of type definitions:

```haskell
data Maybe a = Just a | Nothing
.
data Either a b = Left a | Right b
.         .
data SomeType a = Foo a (Maybe a) | Bar (Either (Maybe a) (SomeType a))
. .........       ...............................
```

or in instance declarations:

```haskell
instance Eq (Maybe a)
.........
instance Eq (SomeType Int)
..............
instance (Eq (Maybe a), Eq (SomeType a)) Eq (Either (Maybe a) (SomeType a))
.........     ............     ...............................
```

Check out examples/tys.textproto for examples.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_var | [TyVar](#lambdabuffers-compiler-TyVar) |  |  |
| ty_app | [TyApp](#lambdabuffers-compiler-TyApp) |  |  |
| ty_ref | [TyRef](#lambdabuffers-compiler-TyRef) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyAbs"></a>

### TyAbs
Only available in TyDef context and not Ty


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_vars | [TyVar](#lambdabuffers-compiler-TyVar) | repeated |  |
| ty_body | [TyBody](#lambdabuffers-compiler-TyBody) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyApp"></a>

### TyApp



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_func | [Ty](#lambdabuffers-compiler-Ty) |  |  |
| ty_args | [Ty](#lambdabuffers-compiler-Ty) | repeated |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyArg"></a>

### TyArg
Type arguments

Type arguments and therefore type variables have kinds, for the purpose of
Lambda Buffers, we only allow them the have kind `Type`.

In the following type definition:

```haskell
Either a b = Left a | Right b
```

both TyArgs `a` and `b` can be assume to be of kind `Type` (zero arity).

However, to allow for future evolution if ever necessary, we attach the Kind
term to TyArgs, even though the Compiler will reject any TyArg that&#39;s not of
kind `Type`. For instance:

```haskell
EitherF (f :: Type -&gt; Type) (a :: Type) (b :: Type) = Left (f a) | Right (f b)
```


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| arg_name | [VarName](#lambdabuffers-compiler-VarName) |  |  |
| arg_kind | [Kind](#lambdabuffers-compiler-Kind) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyBody"></a>

### TyBody
Type body

Lambda Buffers type bodies are quite simple, they can either be:
- `Opaque`, meaning we don&#39;t define its structure with Lambda Buffers but
leverage an existing ones in target languages,
- `Sum of products` which corresponds to Haskell ADTs.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| opaque | [Opaque](#lambdabuffers-compiler-Opaque) |  |  |
| sum | [Sum](#lambdabuffers-compiler-Sum) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyDef"></a>

### TyDef
Type definitions

A type definition consists of a type name, a type arguments and the body.

For instance:

```haskell
data Either a b = Left a | Right b
tttttt a a   bbbbbbbbbbbbbbbb
```

- t - TyName
- a - TyArg
- b - TyBody

One way to look at it is that a type definition introduces a named &#39;lambda
abstraction&#39; in the module scope.
Concretely, `Either` can be considered a type lambda of kind `Type -&gt; Type -&gt;
Type`.
In fact, TyDefs are the only way to introduce such &#39;higher kinded&#39; types.

Once introduced in the module scope, TyDefs are referred to using TyRef term.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |
| ty_abs | [TyAbs](#lambdabuffers-compiler-TyAbs) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyName"></a>

### TyName
Regex [A-Z]&#43;[A-Za-z0-9_]*


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyRef"></a>

### TyRef



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| local_ty_ref | [TyRef.Local](#lambdabuffers-compiler-TyRef-Local) |  |  |
| foreign_ty_ref | [TyRef.Foreign](#lambdabuffers-compiler-TyRef-Foreign) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-TyRef-Foreign"></a>

### TyRef.Foreign



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |
| module_name | [ModuleName](#lambdabuffers-compiler-ModuleName) |  |  |






<a name="lambdabuffers-compiler-TyRef-Local"></a>

### TyRef.Local



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ty_name | [TyName](#lambdabuffers-compiler-TyName) |  |  |






<a name="lambdabuffers-compiler-TyVar"></a>

### TyVar



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| var_name | [VarName](#lambdabuffers-compiler-VarName) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |






<a name="lambdabuffers-compiler-Tys"></a>

### Tys



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ties | [Ty](#lambdabuffers-compiler-Ty) | repeated |  |






<a name="lambdabuffers-compiler-VarName"></a>

### VarName
Regex [a-z]&#43;
Regex [a-z]&#43;


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  |  |
| source_info | [SourceInfo](#lambdabuffers-compiler-SourceInfo) |  |  |





 


<a name="lambdabuffers-compiler-Kind-KindRef"></a>

### Kind.KindRef


| Name | Number | Description |
| ---- | ------ | ----------- |
| KIND_REF_UNSPECIFIED | 0 |  |
| KIND_REF_TYPE | 1 |  |


 

 

 



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
