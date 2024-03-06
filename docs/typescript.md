# LambdaBuffers to Typescript
This chapter will walk through a translation from a LambdaBuffers' module into
    a Typescript module which includes the translation of types.

To demonstrate this, we will use the `lbf-prelude-to-typescript` CLI tool which is just a convenient wrapper over the raw `lbf` CLI.
To this end, we may enter a development shell which provides many other Lambda Buffers CLI tools as follows.

```shell
$ nix develop github:mlabs-haskell/lambda-buffers#lb
$ lbf<tab>
lbf                          lbf-plutus-to-haskell        lbf-plutus-to-rust           lbf-prelude-to-haskell       lbf-prelude-to-rust
lbf-list-modules-typescript  lbf-plutus-to-purescript     lbf-plutus-to-typescript     lbf-prelude-to-purescript    lbf-prelude-to-typescript

```

Or, we may directly refer to the `lbf-prelude-to-typescript` CLI with the following command.

```shell
nix run github:mlabs-haskell/lambda-buffers#lbf-prelude-to-typescript
```

In this chapter, we will use the former option.

Consider the [Document.lbf](./examples/Document.lbf) schema which we may recall is as follows.

```purescript
module Document

-- Importing types
import Prelude (Text, List, Set, Bytes)

-- Author
sum Author = Ivan | Jovan | Savo

-- Reviewer
sum Reviewer = Bob | Alice

-- Document
record Document a = {
  author : Author,
  reviewers : Set Reviewer,
  content : Chapter a
 }

-- Chapter
record Chapter a = {
  content : a,
  subChapters : List (Chapter a)
 }

-- Some actual content
sum RichContent = Image Bytes | Gif Bytes | Text Text

-- Rich document
prod RichDocument = (Document RichContent)
```

We generate the corresponding Typescript code with the following commands.

```shell
$ nix develop github:mlabs-haskell/lambda-buffers#lb
$ lbf-list-modules-typescript lbf-document=. > packages.json 
$ lbf-prelude-to-typescript --gen-opt="--packages packages.json" Document.lbf
$ find autogen/
autogen/
autogen/LambdaBuffers
autogen/LambdaBuffers/Document.mts
autogen/build.json
```

The generated `autogen` directory created contains the generated TypeScript modules.

Note that `lbf-list-modules-typescript` is needed to create a JSON object which maps package names (for NPM) to Lambda Buffers' modules.
Thus, in this example one should have a `package.json` file which associates the key `"name"` with the string value `"lbf-document"`.

The `autogen/build.json` file can be ignored.

The file `autogen/LambdaBuffers/Document.mts` contains the outputted TypeScript module:

```ts
// @ts-nocheck
import * as LambdaBuffers$Document from './Document.mjs'
import * as LambdaBuffers$Prelude from './Prelude.mjs'

export type Author = | { name : 'Ivan' }
                     | { name : 'Jovan' }
                     | { name : 'Savo' }
export const Author : unique symbol = Symbol('Author')
export type Chapter<$a> = { content : $a
                          , subChapters : LambdaBuffers$Prelude.List<Chapter<$a>>
                          }
export const Chapter : unique symbol = Symbol('Chapter')
export type Document<$a> = { author : Author
                           , reviewers : LambdaBuffers$Prelude.Set<Reviewer>
                           , content : Chapter<$a>
                           }
export const Document : unique symbol = Symbol('Document')
export type Reviewer = | { name : 'Bob' } | { name : 'Alice' }
export const Reviewer : unique symbol = Symbol('Reviewer')
export type RichContent = | { name : 'Image'
                            , fields : LambdaBuffers$Prelude.Bytes
                            }
                          | { name : 'Gif'
                            , fields : LambdaBuffers$Prelude.Bytes
                            }
                          | { name : 'Text'
                            , fields : LambdaBuffers$Prelude.Text
                            }
export const RichContent : unique symbol = Symbol('RichContent')
export type RichDocument = Document<RichContent>
export const RichDocument : unique symbol = Symbol('RichDocument')
```

## Product types
The type `RichDocument` have been declared as a product type in the LambdaBuffers schema using the `prod` keyword.

In general, product types are mapped to [tuple types](https://www.typescriptlang.org/docs/handbook/2/objects.html#tuple-types) in TypeScript most of the time. The exception is if there is only one element in the tuple in which case the type is translated to a type alias.

More precisely, given a LambdaBuffers' product type as follows

```purescript
prod MyProduct = SomeType1 ... SomeTypeN
```

where the `...` denotes iterated `SomeTypei` for some `i`, then

- If `N = 0` so `prod MyProduct =`, then we map this to the TypeScript type

  ```ts
  export type MyProduct = []
  ```

- If `N = 1` so `prod MyProduct = SomeType1`, then we map this to the TypeScript type

  ```ts
  export type MyProduct = SomeType1
  ```

  i.e., `MyProduct` simply aliases `SomeType1`

- If `N >= 2` so `prod MyProduct = SomeType1 ... SomeTypeN`, then we map this to the TypeScript type

  ```ts
  export type MyProduct = [SomeType1, ..., SomeTypeN]
  ```

  i.e., `MyProduct` is a tuple with a fixed number of elements with known types.

## Sum types
The types `Author`, `Reviewer`, and `RichContent` have been declared as sum types in the LambdaBuffers schema using the `sum` keyword.

In general, sum types are mapped to a [union type](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types) in TypeScript and follows the additional following rules.
Given a LambdaBuffers' sum type as follows

```purescript
sum MySum
    = Branch1 Branch1Type1 ... Branch1TypeM1
    | ...
    | BranchN BranchNType1 ... BranchNTypeMN
```

where the `...` denotes either an iterated `Branchi` for some `i`, or an
iterated `BranchiTypej` for some `i` and `j`, then each branch, say `Branchi` is translated as
follows.

- If `Branchi` has no fields i.e., `| Branchi`, then the corresponding TypeScript type's union member is

  ```ts
  | { name: 'Branchi' }
  ```

- If `Branchi` has one or more fields i.e., `| Branchi BranchiType1 ... BranchiTypeMi`, then the corresponding TypeScript type's union member is

  ```ts
  | { name: 'Branchi' 
    , fields: <Product translation of BranchiType1 ... BranchiTypeMi>
    }
  ```

  where `<Product translation of BranchiType1 ... BranchiTypeMi>` denotes the
  right hand side of the [product translation](#product-types) of `prod FieldsProduct = BranchiType1 ... BranchiTypeMi`.

  So, for example, given `| Branchi BranchiType1`,  the corresponding TypeScript type is as follows

  ```ts
  | { name: 'Branchi'
    , fields: BranchiType1
    }
  ```

  And given `| Branchi BranchiType1 BranchiType2`, the corresponding Typescript type is as follows.

  ```ts
  | { name: 'Branchi'
    , fields: [BranchiType1, BranchiType2]
    }
  ```

## Record types
The types `Document` and `Chapter` have been declared as record types in the
LambdaBuffers schema using the `record` keyword.

Record types are mapped to [object types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#object-types) in TypeScript.
Given a LambdaBuffers' record type as follows

```purescript
record MyRecord = { field1: SomeType1, ..., fieldN: SomeTypeN }
```

where `...` denotes iterated `fieldi: SomeTypei` for some `i`, the corresponding TypeScript type is

```ts
type MyRecord = { field1: SomeType1, ..., fieldN, SomeTypeN }
```

## Type classes
TypeScript has no builtin implementation of type classes. As such, LambdaBuffers rolled its own type classes.
A complete usage example can be found in the [TypeScript Prelude sample project](./typescript-prelude/src/index.mts).

A type class in TypeScript is an object type which defines a set of methods.
For example, the `Eq` type class in Haskell defines `==` (equality) and `/=` (inequality) as follows.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

Thus, the [`Eq` class](https://github.com/mlabs-haskell/prelude-typescript/blob/main/src/Lib/Eq.ts) in TypeScript is:

```ts
export interface Eq<A> {
  readonly eq: (l: Readonly<A>, r: Readonly<A>) => boolean;
  readonly neq: (l: Readonly<A>, r: Readonly<A>) => boolean;
}
```

Each type class in TypeScript must have an associated global variable which maps unique representations of its instance types to the corresponding object of the type class implementation.
For example, the `Eq` type class has the [global variable](https://github.com/mlabs-haskell/lambda-buffers/blob/main/runtimes/typescript/lbr-prelude/src/LambdaBuffers/Eq.ts#L11) defined in the [lbr-prelude](https://github.com/mlabs-haskell/lambda-buffers/tree/main/runtimes/typescript/lbr-prelude) library defined as follows

```ts
export const Eq: EqInstances = { } as EqInstances
```

where `EqInstances` is an interface type that is initially empty but will be extended with instances of types later.

```ts
export interface EqInstances { }
```

Finally, the following invariant is maintained in the code generator:

- Every type `T` has an associated unique [symbol](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol) also called `T`.

So, the type `Integer` has

```ts
export type Integer = bigint
export const Integer: unique symbol = Symbol('Integer')
```

and implementing its `Eq` instance amounts to the following code.

```ts
export interface EqInstances {
    [Integer]: Eq<Integer>
}
Eq[Integer] = { eq: (l,r) => l === r
              , neq: (l,r) => l !== r
              }
```

For types defined in the LambdaBuffers schema, this chunk of code will be automatically generated provided there is an appropriate `derive` construct.

### Type instances with constraints
Recall in Haskell that the `Eq` instance for a tuple may be defined as follows

```haskell
instance (Eq a, Eq b) => Eq (MyPair a b) where
    MyPair a1 a2 == MyPair b1 b2 = a1 == b1 && a2 == b2
    MyPair a1 a2 != MyPair b1 b2 = a1 != b1 || a2 != b2
```

The corresponding TypeScript type definition and instance would be defined as follows

```ts
export type MyPair<a, b> = [a, b]
export const MyPair: unique symbol = Symbol('MyPair')

export interface EqInstances {
    [MyPair]: <A,B>(a : Eq<A>, b : Eq<B>) => Eq<MyPair<A,B>>
}
Eq[MyPair] = (dictA, dictB) => { return { eq: (a,b) => dictA.eq(a[0], b[0]) && dictB.eq(a[1], b[1])
                                        , neq: (a,b) => dictA.neq(a[0], b[0]) || dictB.neq(a[1], b[1])
                                        } 
                               }
```

Note that the constraints `(Eq a, Eq b) =>` become arguments `dictA` and `dictB` that are used to construct the `Eq` instance for `MyPair`.

This loosely follows the original translation given in the paper [How to make ad-hoc polymorphism less ad hoc](https://dl.acm.org/doi/10.1145/75277.75283) with some minor modifications.

## Limitations

- Only Haskell 2010 typeclasses are supported for the TypeScript code
  generator. So, the following schemas will probably generate incorrect
  code.

  ```purescript
  derive Eq a => Eq (MyPair a a)
  ```
