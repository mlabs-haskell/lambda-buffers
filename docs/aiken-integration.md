# Aiken Integration Report
This document describes LambdaBuffers integration with Aiken.
It appears at Aiken's best,
while it is possible to provide limited support for a subset of LambdaBuffers' features,
integration with LambdaBuffers would provide a poor user experience that would be in conflict with either the major language features of Aiken or the key functionalities provided by LambdaBuffers.
As such, this document describes the challenges LambdaBuffers integration with Aiken,
and proposes alternative Milestone 4 outputs to better foster adoption of LambdaBuffers within the Cardano ecosystem.

## Aiken limitations

This section describes limitations with Aiken.

### Aiken has no type class support

A key feature of LambdaBuffers is to provide both types and type class instances.
Aiken has no support for type classes, so one must generate the type class system themselves.
In other words, one must provide:

* A calling convention for functions with type classes.

* Functions to create instance dictionaries for instances of the type class.

The former is straightforward to implement.
One can explicit pass an instance dictionary for each function that requires a type class as per the usual compilation of type class code to type class free code.

The latter requirement poses troubles for Aiken.
Aiken does not allow one to create instance dictionaries (product types of the instance's methods) since composite types in Aiken cannot contain a function type.

For example, the following Aiken type

```ruost
type EqDict<a> {
    eq: fn(a,a) -> Bool, 
    neq: fn(a,a) -> Bool, 
}
```

would produce an error like

```bash
$ aiken build
    Compiling me/package 0.0.0 (/aikentest/package)
    Compiling aiken-lang/stdlib 1.8.0 (/aikentest/package/build/packages/aiken-lang-stdlib)
        Error aiken::check::illegal::function_in_type

  × While trying to make sense of your code...
  ╰─▶ I found a type definition that has a function type in it. This is not allowed.

   ╭─[/aikentest/package/validators/myvalidator.ak:3:1]
 3 │ type EqDict<a> {
 4 │     eq: fn(a,a) -> Bool,
   ·     ───────────────────
 5 │     neq: fn(a,a) -> Bool,
   ╰────
  help: Data-types can't hold functions. If you want to define method-like functions, group the type definition and the methods under a common namespace in a standalone
        module.

      Summary 1 error, 0 warnings
```

This makes it impossible to pass instance dictionaries via Aiken's builtin types for type classes.

Alternatively, one could try to sidestep Aiken's builtin types by creating a type which is a Church encoded tuple
(i.e., implementing a tuple type via function types),
but doing so requires higher ranked types which again Aiken does not support.
Moreover, it appears that Aiken does not provide any "back doors" to the type system (e.g. TypeScript's `any` type) to trick the type system that using a Church encoded tuple and with its projections are well typed.

It's clear now that having an explicit type for an instance dictionary is not feasible in Aiken,
so owing to the fact that an instance dictionary is a product type of functions, one can replace all instance dictionaries as an argument with multiple arguments with each method, and replace the functions to create an instance dictionary with multiple functions to create each method in the type class.
This is indeed possible in Aiken, and to demonstrate this technique, consider the following translation.

```rust
use aiken/builtin as builtin
use mypackage/lb_prelude/types_/int as lb_prelude_int  // this would have to be implemented in an lb-prelude runtime

pub type MyOption<t> {
    MyJust(t)
    MyNothing
}

pub fn eq(eqt : fn(t,t) -> Bool) -> fn(MyOption<t>,MyOption<t>) -> Bool {
    fn(a,b) { 
        when a is {
            MyJust(aJust) -> when b is {
                MyJust(bJust) -> eqt(aJust,bJust)
                MyNothing -> False
            }
            MyNothing -> when b is {
                MyJust(_) -> False
                MyNothing -> True
            }
        }
    }
}

pub fn fromData(fromDataT : fn(Data) -> t) -> fn(Data) -> MyOption<t> {
    fn(theData) {
        let answer = 
            builtin.choose_data (theData, 
                  (fn(theData) { 
                        let constr = builtin.un_constr_data(theData)
                        let tag = constr.1st
                        let fields = constr.2nd
                          when tag is {
                                0 -> when fields is {
                                    [ justValue ] -> MyJust(fromDataT(justValue))
                                    _ -> error @"Bad parse"
                                }
                                1 -> when fields is {
                                    [] -> MyNothing
                                    _ -> error @"Bad parse"
                                }
                                _ -> error @"Bad parse"
                            }
                    })(theData), (fn(_theData) { error @"Bad parse" })(theData), (fn(_theData) { error @"Bad parse"})(theData), (fn(_theData){error @"Bad parse"})(theData), (fn(_theData) { error @"Bad parse"})(theData))
        answer
    }
}

pub fn toData(toDataT : fn(t) -> Data) -> fn(MyOption<t>) -> Data {
    fn(theOption) {
        when theOption is {
            MyJust(justValue) -> builtin.constr_data(0, [toDataT(justValue)])
            MyNothing -> builtin.constr_data(1, [])
        }
    }
}

// Example usages:

test from_nothing_test() {
    fromData(lb_prelude_int.fromData)(builtin.constr_data(1, [])) == MyNothing
}

test from_just_test() {
    fromData(lb_prelude_int.fromData)(builtin.constr_data(0, [builtin.i_data(69)])) == MyJust(69)
}

test from_to_nothing_test() {
    toData(lb_prelude_int.toData)(fromData(lb_prelude_int.fromData)(builtin.constr_data(1, []))) == builtin.constr_data(1, [])
}

test from_to_just_test() {
    toData(lb_prelude_int.toData)(fromData(lb_prelude_int.fromData)(builtin.constr_data(0, [builtin.i_data(69)]))) == builtin.constr_data(0, [builtin.i_data(69)])
}
```

This translation of type classes has some limitations such as:

* All type class instances must be defined in the same module that the type is defined in i.e., orphan instances are forbidden.

* Only Haskell2010 type classes would be supported.

### Aiken's encoding of its data is different from LambdaBuffers encoding

All onchain scripts must be compiled to UPLC which must in some method represent the higher level language constructs like data types in the original language.
Often, data types in a higher level language are translated to UPLC's builtin `Data` type which supports types like lists, constructors, integers, bytestrings, and maps.
Note that data which will exist as a datum or redeemer must admit a representation with this `Data` type.

LambdaBuffers chooses a particularly efficient encoding of its data types to `Data` mapping to its target languages that map to UPLC.
For example, a record like

```purescript
record MyRecord = { a : Integer, b : Integer }
```

would be translated to

```purescript
[a, b]
```

i.e., records are lists of all record components[^recordsSpecialCases].

[^recordsSpecialCases]: Singleton records are encoded as just the single element.

If LambdaBuffers compiled `MyRecord` to a [record in Aiken](https://aiken-lang.org/language-tour/custom-types) as follows

```rust
type MyRecord {
    a : Int,
    b : Int
}
```

we know that Aiken will internally represent this as the following `Data` type

```purescript
Constr 0 [a, b]
```

where we note that Aiken includes a useless `Constr 0` tag meaning Aiken's encoding is less efficient than LambdaBuffers' encoding.

In general, Aiken's documentation for the encoding from Aiken's types to UPLC `Data` is unclear,
but one can inspect the generated UPLC to verify that Aiken would encode the data as mentioned above.

For example, given the following Aiken module

```rust
pub type MyRecord { a: Int, b: Int }

validator {
    pub fn hello_world(_redeemer: Data, _scriptContext: Data) {
        let theRecord = MyRecord(69, -69)

        theRecord.a == 420 && theRecord.b == -420
    }
}
```

One can compile and inspect the UPLC as follows

```shell
$ aiken build --uplc
...
$ cat artifacts/myvalidator.hello_world.uplc
artifacts/myvalidator.hello_world.uplc
(program
  1.0.0
  [
    (lam
      i_0
      [
        (lam
          i_1
          [
            (lam
              i_2
              [
                (lam
                  i_3
                  (lam
                    i_4
                    (lam
                      i_5
                      (force
                        [
                          [
                            [
                              i_3
                              (force
                                [
                                  [
                                    [
                                      i_3
                                      [
                                        [
                                          (builtin equalsInteger)
                                          [
                                            (builtin unIData)
                                            [
                                              i_1
                                              [
                                                i_2
                                                [
                                                  (builtin unConstrData)
                                                  (con
                                                    data (Constr 0 [I 69, I -69])
                                                  )
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                        (con integer 420)
                                      ]
                                    ]
                                    (delay
                                      [
                                        [
                                          (builtin equalsInteger)
                                          [
                                            (builtin unIData)
                                            [
                                              i_1
                                              [
                                                i_0
                                                [
                                                  i_2
                                                  [
                                                    (builtin unConstrData)
                                                    (con
                                                      data (Constr 0 [I 69, I -69])
                                                    )
                                                  ]
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                        (con integer -420)
                                      ]
                                    )
                                  ]
                                  (delay (con bool False))
                                ]
                              )
                            ]
                            (delay (con unit ()))
                          ]
                          (delay [ (error ) (force (error )) ])
                        ]
                      )
                    )
                  )
                )
                (force (builtin ifThenElse))
              ]
            )
            (force (force (builtin sndPair)))
          ]
        )
        (force (builtin headList))
      ]
    )
    (force (builtin tailList))
  ]
)
```

In particular, the following lines are evidence to support that the record is encoded inefficiently as `Constr 0 [<integer>, <integer>]`.

```haskell
      [
        (builtin unConstrData)
        (con
          data (Constr 0 [I 69, I -69])
        )
      ]
```

This is awkward for LambdaBuffers since when Aiken works with the `MyRecord` type,
it is represented with the `Constr` tag meaning that LambdaBuffers' efficient encoding would be translated to Aiken's inefficient encoding.
Ideally, one would want to change how Aiken encodes its data types internally so that Aiken can use LambdaBuffers' efficient encoding everywhere.
Thus, we lose all benefits of LambdaBuffers' efficient encoding when working with Aiken's mechanisms to define types because LambdaBuffers is forced to take an extra step to translate to Aiken's inefficient encoding.
As such, Aiken's opinionated way of encoding its data is at odds with LambdaBuffers.

To resolve the mismatch in the encoding of data between the two, one could alternatively sidestep all of Aiken's methods for defining types and instead use Aiken's opaque types to alias `Data` and provide ones own constructors / record accesses as follows

```rust
use aiken/builtin as builtin

pub opaque type MyRecord { data: Data }

pub fn myRecord(a: Int) -> MyRecord {
    MyRecord{ data : builtin.list_data([builtin.i_data(a)]) }
}

pub fn myRecord_a(value : MyRecord) -> Int {
    builtin.un_i_data(builtin.head_list(builtin.un_list_data(value)))
}

// Example program:
validator {
    pub fn hello_world(_redeemer: Data, _scriptContext: Data) {
        let theRecord = myRecord(69)

        myRecord_a(theRecord) == 420
    }
}
```

Interested readers may inspect the compiled UPLC to verify that the data encoding of `MyRecord` agrees with LambdaBuffers' encoding.

```purescript
(program
  1.0.0
  [
    (lam
      i_0
      [
        (lam
          i_1
          [
            (lam
              i_2
              (lam
                i_3
                (lam
                  i_4
                  (force
                    [
                      [
                        [
                          i_2
                          [
                            [
                              (builtin equalsInteger)
                              [
                                (builtin unIData)
                                [
                                  i_0
                                  [
                                    [ i_1 (con data (I 69)) ]
                                    (con (list data) [])
                                  ]
                                ]
                              ]
                            ]
                            (con integer 420)
                          ]
                        ]
                        (delay (con unit ()))
                      ]
                      (delay [ (error ) (force (error )) ])
                    ]
                  )
                )
              )
            )
            (force (builtin ifThenElse))
          ]
        )
        (force (builtin mkCons))
      ]
    )
    (force (builtin headList))
  ]
)
```

Note that this would most likely offer a poor user experience as this would essentially replace a large part of Aiken's language construct with our own generated functions for constructing, deconstructing, serialization / deserialization to `Data`, etc.

### Aiken's packages only support fetching dependencies remotely

LambdaBuffers is more than just a code generator.
In addition to generating code for sharing types and semantics, its Nix tools augment a set of packages for a project with a package of the generated LambdaBuffers code.

Aiken does support having packages, but it appears that it only officially supports fetching packages from either Github, GitLab, or BitBucket i.e., it's unclear how to create a local package set augmented with LambdaBuffers' packages.

For example, the following `aiken.toml` file

```toml
name = "me/package"
version = "0.0.0"
plutus = "v2"
license = "Apache-2.0"
description = "Aiken contracts for project 'package'"

[repository]
user = "me"
project = "package"
platform = "github"

[[dependencies]]
name = "aiken-lang/stdlib"
version = "1.8.0"

source = "github"
[[dependencies]]
name = "me/otherpackage"
version = "0.0.0"
source = "what do I put here if I have my own local package?"
```

would produce an error like

```bash
$ aiken build
        Error aiken::loading::toml

  × TOML parse error at line 20, column 10
  │    |
  │ 20 | source = "what do I put here if I have my own local package?"
  │    |          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  │ unknown variant `what do I put here if I have my own local package?`, expected one of `github`, `gitlab`, `bitbucket`
  │
    ╭─[/home/jared/Documents/Work/aikentest/mypackage/aiken.toml:19:1]
 19 │ version = "0.0.0"
 20 │ source = "what do I put here if I have my own local package?"
    ·          ────────────────────────────────────────────────────
 21 │
    ╰────
```

where the error message makes it clear that it only expects the source of dependencies to be from either GitHub, GitLab, or BitBucket.

As such, it's unclear how to augment the local set of packages with a LambdaBuffers package, as the Nix tools would provide a local package.
Indeed, it's most likely possible to trick Aiken into thinking that a LambdaBuffers package is already installed,
but this delves into implementation specific details of Aiken that may break between releases.

## Alternative milestone 4 outputs

Seeing the aforementioned incompatibilities between Aiken and LambdaBuffers,
instead of hacking around the foundational design decisions of Aiken and LambdaBuffers to create an Aiken backend with limited support and a poor user experience,
we strongly believe that milestone 4 would be better spent to improve the existing LambdaBuffers stack.
In particular, LambdaBuffers has seen use in other projects such as [DeNS](https://github.com/mlabs-haskell/DeNS/tree/main), OrcFax, etc.
and we've received feedback to better the LambdaBuffers existing facilities so addressing this feedback would aid in fostering adoption of LambdaBuffers in other projects.

As such, for milestone 4, we propose to provide the following instead:

Bugs:

* Haskell backend bugs.

  * [Generated Haskell code is invalid](https://github.com/mlabs-haskell/lambda-buffers/issues/197)

  * [Missing dependencies from the generated files](https://github.com/mlabs-haskell/lambda-buffers/issues/124)

* Plutarch backend bugs.

  * [Generated Plutarch code is invalid](https://github.com/mlabs-haskell/lambda-buffers/issues/148)

* [Optimizing the LambdaBuffers compiler performance](https://github.com/mlabs-haskell/lambda-buffers/issues/76)

Features:

* [Completing the Plutus `.lbf` schemas to include all Plutus Ledger API types](https://github.com/mlabs-haskell/lambda-buffers/issues/175)

* [Creating a versioning scheme](https://github.com/mlabs-haskell/lambda-buffers/issues/220)

* [Separate the PlutusTx backend from a Haskell Plutus backend](https://github.com/mlabs-haskell/lambda-buffers/issues/221)

* [Optimizing slow nix build times](https://github.com/mlabs-haskell/lambda-buffers/pull/193#issuecomment-1942114795)

* [Improving error messages for better editor integration](https://github.com/mlabs-haskell/lambda-buffers/issues/152)
