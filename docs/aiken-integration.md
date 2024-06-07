# Lambda Buffers: Aiken Research Document

The Lambda Buffers team has deeply researched the Aiken programming language with the intention to find a technical path to integrate it with Lambda Buffers along the already integrated languages Plutus, Haskell, Rust and JavaScript.
The conclusion of this research phase is that, while it would be indeed possible for Lambda Buffers to provide limited support for Aiken, it would result in a poor user experience that would be in conflict with the major language features of Aiken or in conflict with the key functionalities provided by Lambda Buffers.
This document presents in detail the challenges found and its impact on the feasibility or convenience to undertake the Aiken integration.

## Aiken limitations

This section describes limitations with Aiken.

All testing / observations with Aiken were done with the following version.

```bash
$ aiken --version
aiken v1.0.28-alpha+c9a1519
```

### Aiken has no type class support

A key feature of Lambda Buffers is to provide both types and type class instances.
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

would produce an error as follows.

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
Moreover, it appears that Aiken does not provide any "back doors" to the type system (e.g. TypeScript's `any` type) to trick the type system that using a Church encoded tuple and its projections are well typed.

It's clear now that having an explicit type for an instance dictionary is not feasible in Aiken,
so owing to the fact that an instance dictionary is a product type of functions, one can achieve type classes via dictionary passing by replacing all instance dictionaries as multiple arguments of each method in the type class, and replace the function to create an instance dictionary with multiple functions to create each method in the type class.
This is indeed possible in Aiken, and to demonstrate this technique, consider the following Haskell code (which loosely models code generated from Lambda Buffers)

```haskell
class Eq a where
    eq :: a -> a -> Bool

class PlutusData a where
    toData :: a -> Data
    fromData :: Data -> a

data MyOption a = MyJust a | MyNothing

instance Eq a => Eq (MyOption a) where
    eq (MyJust s) (MyJust t) = s == t
    eq MyNothing  MyNothing  = True
    eq _          _          = False

instance PlutusData a => PlutusData (MyOption a) where
    toData (MyJust s) = Constr 0 [toData s]
    toData MyNothing  = Constr 1 []

    fromData (Constr 0 [s]) = MyJust (fromData s)
    fromData (Constr 1 [])  = MyNothing
    fromData _              = error "bad parse"
```

A translation to type class free code in Aiken is as follows.

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

While the above has demonstrated how one can translate type class instances in Lambda Buffers to type class free code in Aiken,
this unfortunately leads to a bad user experience for the "builtin" PlutusData type class in Aiken.
Aiken by default "generates" its own PlutusData instances for all composite types.
As such, Aiken provides some nice syntactic features to make writing smart contracts particularly readable.

A common pattern to write a validator in Aiken is as follows.

```rust
pub type MyRecord<t> {a : t, b : Int }

validator {
    pub fn hello_world(_redeemer: MyRecord<Int>, _scriptContext: Data) {
                              //  ^~~~ this will automatically use Aiken's builtin PlutusData instances
        ...
    }
}
```

Unfortunately, with the type class system described in this section,
an Aiken developer will no longer be able to write this (since Lambda Buffers would generate its own PlutusData instances that are not used by Aiken)
and instead must write the validator more verbosely as follows.

```rust
pub type MyRecord<t> {a : t, b : Int }

validator {
    pub fn hello_world(redeemer: Data, _scriptContext: Data) {
       let actualRedeemer = myRecordFromData(intFromData)(redeemer)
        // ^~~~ Aiken users need to write more code in order to use Lambda
        // Buffers so that it will use Lambda Buffers' encoding for the
        // validator. Note that this sample assumes that `myRecordFromData :: (Data -> t) -> Data -> MyRecord<t>` 
        // exists as would be generated by Lambda Buffers.
        ...
    }
}
```

Clearly, this increase in code bloat to express the same simple idea contradicts the promises of making smart contracts easy to write on Aiken.

### Aiken's encoding of its data is different from Lambda Buffers encoding

All onchain scripts must be compiled to UPLC which must in some method represent the higher level language constructs like data types in the original language.
Often, data types in a higher level language are translated to UPLC's builtin `Data` type which supports types like lists, constructors, integers, bytestrings, and maps.
Note that data which will exist as a datum or redeemer must admit a representation with this `Data` type.

Lambda Buffers chooses a particularly efficient encoding of its data types to `Data` mapping to its target languages that map to UPLC.
For example, a record like

```purescript
record MyRecord = { a : Integer, b : Integer }
```

would be translated to

```purescript
[a, b]
```

i.e., records are lists of all record components[^recordsSpecialCases].

[^recordsSpecialCases]: There are some special cases for the encoding in Lambda Buffers. For example, singleton records are encoded as just the single element.

If Lambda Buffers compiled `MyRecord` to a [record in Aiken](https://aiken-lang.org/language-tour/custom-types) as follows.

```rust
type MyRecord {
    a : Int,
    b : Int
}
```

Then, one can observe that Aiken will internally represent this as the following `Data` type

```purescript
Constr 0 [a, b]
```

where we note that Aiken includes a useless `Constr 0` tag meaning Aiken's encoding is less efficient than Lambda Buffers' encoding.

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

This is awkward for Lambda Buffers since when Aiken works with the `MyRecord` type,
it is represented with the `Constr` tag meaning that Lambda Buffers' efficient encoding would be translated to Aiken's inefficient encoding.
Ideally, one would want to change how Aiken encodes its data types internally so that Aiken can use Lambda Buffers' efficient encoding everywhere.
Thus, we lose all benefits of Lambda Buffers' efficient encoding when working with Aiken's mechanisms to define types because Lambda Buffers is forced to take an extra step to translate to Aiken's inefficient encoding.
As such, Aiken's opinionated way of encoding its data is at odds with Lambda Buffers.

To resolve the mismatch in the encoding of data between the two,
one could alternatively sidestep all of Aiken's methods for defining types
and instead use Aiken's opaque types to alias `Data` and provide ones own constructors / record accesses as follows.

```rust
use aiken/builtin as builtin

pub opaque type MyRecord { data: Data }

// Constructor for `MyRecord`
pub fn myRecord(a: Int, b: Int) -> MyRecord {
    MyRecord{ data : builtin.list_data([builtin.i_data(a), builtin.i_data(b)]) }
}

// Projection for the field `a` of `MyRecord`
pub fn myRecord_a(value : MyRecord) -> Int {
    builtin.un_i_data(builtin.head_list(builtin.un_list_data(value)))
}

// Projection for the field `b` of `MyRecord`
pub fn myRecord_b(value : MyRecord) -> Int {
    builtin.un_i_data(builtin.head_list(builtin.tail_list(builtin.un_list_data(value))))
}

validator {
    pub fn hello_world(_redeemer: Data, _scriptContext: Data) {
        let theRecord = myRecord(69, -69)

        myRecord_a(theRecord) == 420 && myRecord_b(theRecord) == -420
    }
}
```

Interested readers may inspect the compiled UPLC to verify that the data encoding of `MyRecord` agrees with Lambda Buffers' encoding.

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
                              [
                                (lam
                                  i_6
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
                                                  [ (builtin unListData) i_6 ]
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
                                                    [ (builtin unListData) i_6 ]
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
                                )
                                [
                                  (builtin listData)
                                  [
                                    [ i_2 (con data (I 69)) ]
                                    [
                                      [ i_2 (con data (I -69)) ]
                                      (con (list data) [])
                                    ]
                                  ]
                                ]
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
    (force (builtin tailList))
  ]
)
```

Note that this would most likely offer a poor user experience as this would essentially replace a large part of Aiken's language construct with our own generated functions for constructing, deconstructing, serialization / deserialization to `Data`, etc.

In either case,
to mediate the Data serialization / deserialization mismatch of Aiken and Lambda Buffers,
it puts a bulkier mental overhead on the Aiken developer.
For example, as in the previous section, an Aiken developer would expect to write a validator as follows.

```rust
pub type MyRecord {a : Int, b : Int }

validator {
    pub fn hello_world(_redeemer: MyRecord, _scriptContext: Data) {
                              //  ^~~~ this will automatically use Aiken's builtin Data serialization and deserialization
        ...
    }
}
```

But, any of the solutions to mediate the Data encoding mismatch of Aiken and Lambda Buffers would force an Aiken developer to instead write a more verbose validator as follows.

```rust
pub type MyRecord {a : Int, b : Int }

validator {
    pub fn hello_world(redeemer: Data, _scriptContext: Data) {
        let actualRedeemer = myRecordFromData(redeemer)
                          // ^~~~ Assume that Lambda Buffers would generate `myRecordFromData :: Data -> MyRecord`
        ...
    }
}
```

Again, it's clear this contradicts Aiken's goal of making writing smart contracts easy as Lambda Buffers integration would increase the mental overhead of working with all generated data types.

### Aiken's packages only support fetching dependencies remotely

Lambda Buffers is more than just a code generator.
In addition to generating code for sharing types and semantics, its Nix tools augment a set of packages for a project with a package of the generated Lambda Buffers code.

Aiken does support having packages, but it appears that it only officially supports fetching packages from either Github, GitLab, or BitBucket i.e., it's unclear how to create a local package set augmented with Lambda Buffers' packages.

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

As such, it's unclear how to augment the local set of packages with a Lambda Buffers package.
Indeed, it's possible to trick Aiken into thinking that a Lambda Buffers package is already installed by preparing Aiken's build directory with the dependencies copied in ahead of time,
but this delves into implementation specific details of Aiken that may break between releases.
An example of this technique is [here](https://github.com/mlabs-haskell/uplc-benchmark/blob/master/nix/aiken/lib.nix#L83).

## Summary of Aiken limitations

This section summarizes the Aiken limitations and incompatibilities with Lambda Buffers.

1. Aiken has no type classes, but Lambda Buffers requires type classes. As such, Lambda Buffers support for Aiken would require its own implementation of type classes.
   Unfortunately, implementing type classes is awkward in Aiken because composite data types in Aiken cannot store functions.
   While there are work arounds to implement type classes in Aiken,
   this fundamentally will create a poor user experience for an Aiken developer as using Lambda Buffers' generated type classes such as PlutusData would be at odds with the builtin syntactic goodies of Aiken's default PlutusData type class instances.

2. Aiken's PlutusData representation of its data types is different from Lambda Buffers' representation of PlutusData.
   This means that we have a choice of either:

   * Translating Lambda Buffers types to Aiken's builtin composite types which would lead to inefficient code in the already constrained onchain code environment since this would be "massaging" PlutusData representations when we would really want Aiken to use Lambda Buffers PlutusData encoding directly.

   * Translating Lambda Buffers types to a opaque type alias in Aiken which would then require us to generate supporting functions for constructors and destructors which would make Aiken's major language features obsolete, and so have a poor user experience.

   To put this more explicitly, we either have inefficient code with a somewhat nice user experience for an Aiken developer, or efficient code with an awful user experience for an Aiken developer.

3. Creating local package sets in Aiken is unclear, but creating such local package sets is a principle feature of Lambda Buffers.
   Indeed, there are tricks one can do to work around this, but this depends on internal implementation details of Aiken that may break between releases.

All in all, at the moment it's clear that while it may be possible to integrate Aiken with Lambda Buffers, such integration would have

* limited support for Lambda Buffers' key features; and

* a poor user experience for Aiken developers that use Lambda Buffers.

So, the extra effort needed to mitigate these challenges appear to be counter productive with Aiken's and Lambda Buffers' project goals.
Moreover, Aiken is still in an alpha release and is rapidly changing, so the effort to mitigate these challenges would be squandered away as Aiken evolves.
Thus, given these challenges, it's clear that it would be unwise to undertake the Aiken implementation currently,
and it would be wiser to revisit this later and focus on matters of pressing importance today to better foster adoption of Lambda Buffers.

Lambda Buffers has fortunately seen industry use in other projects such as [DeNS](https://github.com/mlabs-haskell/DeNS/tree/main), [TripHut DAO](https://github.com/yaadlabs/DAO-Off-Chain), etc.,
and there's been feedback to improve the existing facilities in Lambda Buffers which would aid in fostering the adoption of Lambda Buffers in the greater Cardano community.
Some of these issues include the following.

* Bugs:

  * Haskell backend bugs.
  
    * [Generated Haskell code is invalid](https://github.com/mlabs-haskell/lambda-buffers/issues/197)
  
    * [Missing dependencies from the generated files](https://github.com/mlabs-haskell/lambda-buffers/issues/124)
  
  * Plutarch backend bugs.
  
    * [Generated Plutarch code is invalid](https://github.com/mlabs-haskell/lambda-buffers/issues/148)
  
  * [Optimizing the Lambda Buffers compiler performance](https://github.com/mlabs-haskell/lambda-buffers/issues/76)

* Features:

  * [Completing the Plutus `.lbf` schemas to include all Plutus Ledger API types](https://github.com/mlabs-haskell/lambda-buffers/issues/175)
  
  * [Creating a versioning scheme](https://github.com/mlabs-haskell/lambda-buffers/issues/220)
  
  * [Separate the PlutusTx backend from a Haskell Plutus backend](https://github.com/mlabs-haskell/lambda-buffers/issues/221)
  
  * [Optimizing slow nix build times](https://github.com/mlabs-haskell/lambda-buffers/pull/193#issuecomment-1942114795)
  
  * [Improving error messages for better editor integration](https://github.com/mlabs-haskell/lambda-buffers/issues/152)
