# Typeclass Support Spec & Design

## Introduction

Other schema languages support generating type definitions in many languages from a single definition in the schema language. One key feature that sets LambdaBuffers apart from these alternatives is support for _typeclasses_, which enable the generation of ad-hoc polymorphic functions that operate on types generated from LambdaBuffers schemata.

Due to inherent limitations in the design of the LambdaBuffers schema language, users will not be able to define typeclass instances themselves. (Allowing users to write their own methods would greatly complicate the schema language to the point where it is doubtful it could rightly be called a schema language.) Users, instead, will write _deriving clauses_ as part of the schema definition, and the LambdaBuffers code generator will derive these declared instances when generating code.

Two important consequences of this design decision are:

1) _All instances must be derived structurally_. As an example, consider the arbitrary product type `data P a b = P a b`. The semantics of the generated instance (i.e. the behavior of the generated code) must be determinable from the _structure of the type_ - that it is a product - and the instances for its arguments `a` and `b`, and by those features alone. (Since `a` and `b` are type variables here, writing a direct instance for any interesting class is likely impossible, so LambdaBuffers supports constrained instances such as `instance (C a, C b) => C (P a b)`)

2) _All instances must be uniform across supported languages_. Because the LambdaBuffers codegen component (and _not_ the user) is responsible for generating instances, we must ensure that the codegen component is suitably equipped to generate instances in each language that exhibit behavior which is, to the greatest extent possible, equivalent to the behavior of generated instances in any other language. We _must_ have an extensive test quite to verify uniform behavior of generated instances.

In languages with a typeclass system (Haskell, PureScript) or equivalent (Rust's Traits), we will utilize the existing system and _should_ (to the extent that doing so is feasible) utilize existing typeclasses and instances from commonly used or standard libraries. In languages lacking a type system that is sufficiently rich to express typeclass relations, we will generate instances using idiomatic language features. (The details will depend on the particular language.)

## Architecture: Constraint Solver & TypeClass Deriving

Implementing typeclasses in LambdaBuffers requires two distinct (but related) components: A constraint solver and a deriver/generator.

### Constraint Solver

#### Definitions & Preliminaries

In order to derive and generate instance declarations in a variety of target languages, we must be able to express _rules_ and _constraints_, and determine whether those rules apply and constraints can be satisfied. The components of a _rule_ are:

  -1) A  _pattern_, which is a _general_ representation into which concrete representations (i.e. types or their structural subcomponents) can be substituted, such that the most specific rule that covers a given type can be selected. Types (or their structural subcomponents) should be transformed into a _concrete pattern_ to facilitate the substitution, where a _concrete pattern_ can be thought of as a general pattern that has already been substituted.

  For an arbitrary concrete pattern (again, representing a type or structural subcomponent thereof) _p_, and a set of general patterns (rules) _rs_, the _most specific match_ can be determined by substituting _p_ into each element of _rs_ (discarding elements of _rs_ for which no substitution can be performed), sorting _rs_ "by specificity" - i.e., such that for two arbitrary elements of _rs_ _r1_ and _r2_, _r1_ is regarded as more specific than _r2_ iff _r1_ is a substitution instance of _r2_.

 -2) A semantic identifier that indicates the typeclass that the pattern serves as a rule _for_. In the absence of superclasses, a String or enumeration would suffice. However, as we intend to support superclasses in LambdaBuffers, this semantic identifier must record information about the superclasses of the class it identifies.

What differentiates a __rule_ from a bare _constraint_ is that a rule may express a dependency on other constraints. Informally, we wish to be able to express rules that assert such things as: "If type _T_ satisfies  _C_, then _List T_ also satisfies _C_"

Implementation:

 ```haskell
data Class (l :: Lang) = Class
  { name   :: String
  , supers :: [Class l]
  }

data Constraint l = C (Class l) Pat

type Instance l = Rule l

data Rule (l :: Lang)  where
  (:<=) :: Constraint l -> [Constraint l] -> Rule l
--         /\ head           /\ context
 ```

As the above code snippet indicates, rules are language specific. This is necessary for two reasons: First, although several of our target languages have an existing typeclass hierarchy (Haskell, PureScript, Rust), the structure of that hierarchy differs in each language. For example, Rust's `Eq` class (trait) is superclassed by a `PartialEq` class (trait), whereas in Haskell `Eq` has no superclasses. Second, because several of our target languages have relatively advanced support for generics, it may be possible to write generic instances (using e.g. SOP generics in Haskell or row-based generic techniques in PureScript) in one language but not in another.

There are, broadly, two different "sorts" of rules (which are both represented by the above type but are differentiated by their function):

- Rules which correspond to typeclass instances (or some equivalent in target languages without typeclass support) that __already exist_. In languages with typeclass support, those instances may be part of a language's prelude, or may be imported from a LambdaBuffers compatibility module. These are things like: `C eq (List _x) :<= [C eq _x]` (where `eq = Class "Eq" []`)

- _Structural_ or _deriving_ rules which do not directly correspond to any existing instance in a language's prelude or any module, but are used to determine whether a valid instance can be constructed by our code generation framework. These are things like: `C eq (RecP _xs) :<= [C eq _xs]` (Where `_xs` is a variable pattern and `RecP` is the constructor for record patterns)

Final note: Although we are not aware of any theoretical issues that would prevent implementing multiple parameter type classes, the presence of one parameter substantially complicates the implementation and leads to the possibility of ambiguity that is not easily resolved (historically, Haskell's implementation of this feature required FunctionalDependencies to allow for disambiguation). We may attempt to implement MPTC support subject to funding/time constraints, but do not regard MPTCs as an essential feature at this time.

#### Logic & Implementation

The constraint solver is implemented in a single function with the type signature:

```haskell
solve :: forall (l :: Lang).
  [Instance l] -> -- A
  Constraint l -> -- B
  [Constraint l]  -- C
```

- The first argument A is a list that represents the set of in-scope instances used to solve the constraint.
- The second B argument represents the constraint that we aim to solve.
- The function returns C either an empty list of constraints, in which case we regard the constraint as having been solved, or a list of subgoal constraints necessary for solving the argument constraint in B.

The constraint solving algorithm is similar to the algorithm that Haskell employs (though it differs in the handling of superclasses, as will be explained below) and is quite simple:

- First, we filter the list of in-scope instances such only elements of the list which have the same class as B and have a head that the pattern in B is a substitution instance of remain.
  - If the filtered list is empty we report failure by returning [B].
  - If the filtered list is not empty, we choose the most specific matching instance rule.
    - If the most specific matching rule does not have any dependencies, attempt to solve all of the superclass constraints, returning [] if each superclass can be solved, reporting the failures if at least one superclass constraint fails.
    - If the most specific matching rule has dependencies, we try to solve the dependencies, failing and reporting the unsolved dependencies if any fail and moving to the previous step if all succeed.

The essential difference between our algorithm and Haskell's constraint solving algorithm is that the set of in-scope instances has already been validated in Haskell, so the inference from subclass -> superclass is valid. Because we directly define our instances using the `Rule` ADT noted above, it is possible that a necessary superclass instance may be missing (e.g. if we forgot to write an instance rule for it). Therefore we check the superclasses __after_ checking the target constraint in B using the same scope we use to check B, and we do so out of necessity: A missing superclass instant would result in generated code that would not compile in the target language (if that target language is compiled), but, more importantly, would result in generated code that throws a runtime error in a dynamic target language.

## Deriving / Generator

### Motivation

The constraint solver can only tell us whether a user-declared instance is _already covered by a set of existing rules_ and, therefore, _does not need to be generated_. As noted above, in languages with very expressive generic programming systems, this may be sufficient for a large number of instances (perhaps all of them!) for certain classes - but it is surely not sufficient for every instance a user might want in every target language. Additionally, while generic programming techniques are very powerful in many target languages, they are not necessarily as performant as directly generated code. Consequently, the constraint solver alone is not sufficient to achieve our aims in every language or for every class we wish to implement, and we need true deriving machinery.

### Definitions and Preliminaries

#### Parameterized Deriving

Because LambdaBuffers (unlike the vast majority of other schema languages) aims to support _parameterized schemata_ -- i.e., schemata with type variables that can represent any type -- it is essential that we allow users to declare _parameterized deriving clauses_. That is: In addition to supporting structural deriving of typeclass instances of the form `derive Eq Foo`, LambdaBuffers must also be able to support parameterized deriving statements of the form `derive (Eq a, Eq b) => Eq (Bar a b)`.

For the sake of parsimony, we represent both non-parameterized and parameterized deriving statements using the `Rule` type outlined in an above code snippet, though in the context of a user-providing deriving statement it has a distinct semantics. When we receive a `derive (Eq a, Eq b) => Eq (Bar a b)` statement from the user and translate it into `C eq (Bar (VarP "a") (VarP "b")) :<= [C eq (VarP "a"), C eq (VarP "b")]`, we interpret this as meaning: "Generate an Eq instance for `Bar a b` that works for any types `a` and `b` which have Eq instances".

#### Maximal Deriving

There are two viable approaches on my take when implementing deriving machinery:

- _Minimal deriving_: Only derive instances for the specific types indicated in the user's deriving clause. Fail whenever a necessary intermediate instance is missing _even in cases where it could be derived_.
- _Maximal deriving_: Derive instances for all types indicated in the user's deriving clause. If an intermediary instance is missing, attempt to derive the intermediary instance as well.

Although many languages use a minimal deriving approach, we opt for the maximal approach in LambdaBuffers. The primary benefit of the minimal approach is that it allows users to explicitly define the methods of typeclass instances in the manner they choose, and are not surprised by the behavior of any derived instances (or by the existence of an unexpected derived instance). While the minimal approach has merit in the context of a fully featured programming language (i.e. one where users can write _functions_ or _methods_), in a schema language like LambdaBuffers we require all supported typeclasses to be derived _uniformly_ based on the _structure expressed by the LamdaBuffers schemata_.

We assume that if a user desires a typeclass instance for some composite type, they also desire the intermediate instances for each of the types structurally contained by that type. _This must be clearly communicated in the user facing documentation_.

#### Deriving Strategies (WIP/Not finalized/Needs discussion)

We _should_ provide users the option to use two different deriving strategies where appropriate: `stock` and `newtype`, defaulting to `stock` if no strategy is provided.

Although we do not (and do not plan to) support `newtype`s in the schema language (i.e. we do not allow users to specify the 'newtypeyness' of a generated type in their schemata, though we may decide to generate `newtype`s in a target language in accordance with a standard set of criteria), the ability to specify a `newtype` deriving strategy is a powerful tool, especially for typeclasses that deal with encodings.

As is the case in Haskell, a `newtype` deriving strategy is one where the outermost constructor is ignored for single-constructor types.

This is most relevant when it comes to the `PlutusData` (and, to a lesser extent `JSON`) typeclass, where we cannot sensibly default to a `stock` or `newtype` strategy without producing behavior which may be unexpected or deleterious to the user experience: If we default to a `stock` strategy, then we force users to accept additional overhead resulting from the `Constr` wrapper for every generated type. However, we cannot default to a `newtype` strategy where appropriate either - even though it is trivial to detect "newtypey" schemata, the `newtype` encoding introduces the possibility of breaking changes should a user want to add an additional constructor to the on-chain representation of their schema in a future version of their protocol.

#### Definitions & Types

The deriving function is a stateful computation and uses the following state type (to be explained below):

```haskell
type GenTable l = Map (Instance l) (InstanceGen l) -- A

data DeriveState (l :: Lang) = DeriveState {
  scope      :: [Instance l],                      -- B
  generators :: GenTable l,
  output     :: [DSL l]                            -- C
}

```

- _A_ We assume that we have a table of rules and code generation functions which operate on any concrete pattern that is a substitution instance of those rules. The implementation of the code generation functions is not relevant here. The content of this table remains static throughout the deriving process.

- _B_ We assume that we have a set of instance rules representing the set of in-scope instances. The content of this set _does_ change throughout the deriving process.

- _C_ We collect the results of the code generation in a list. `DSL l` is simply an associated type family that indicates the structured output for a given target language. It may be something as simple as `Text` or `Doc`, or it may be a complex ADT that encodes more of the structure of the target language.

### Logic & Implementation

The deriving algorithm is, once again, quite simple.

When we receive a deriving statement from the frontend, we first split the statement into the 'head' constraint and the 'context' constraints (should any exist) expressed in the statement. Then we add the 'context' constraints to the set of in-scope instance rules as assumptions and follow the following procedure on the 'head' constraint:

- Check whether the instance to be derived is already covered by the existing in-scope rules
  - If yes: Do nothing, report success.
  - If no: Try to derive all subgoal constraints emitted by the solver.
    - Check the generator table for a matching instance head.
      - If there is no matching head, fail
      - If there is a matching head:
        - If the head has no dependency constraints, run the generator on the type and record the output
        - If the head has dependency constraints, attempt to derive them
          - If any subgoal constraint cannot be derived, fail and report the failure.
          - If all subgoal constraints have been derived, add them to the set of existing instances and attempt derivation again for the original target.
