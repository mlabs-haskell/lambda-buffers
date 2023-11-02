# LambdaBuffers Frontend (.lbf) syntax

The input to the LambdaBuffers Frontend is a text file which contains a module that defines a specification of the types and type class instances you want to generate. This chapter gives the exact syntax of a LambdaBuffers Frontend file, and informally describes meaning of the syntactic constructs.

The name of a LambdaBuffers Frontend file must end with `.lbf`, and hence may also be referred to as a .lbf file or a .lbf schema.

## Notation

In the following description of a LambdaBuffers Frontend file's syntax, we use a similar BNF syntax from [Section 10.1 of the Haskell Report](https://www.haskell.org/onlinereport/haskell2010/). So, the following notational conventions are used for presenting syntax.

|  Syntax       | Description                                                                 |
| ------------- | --------------------------------------------------------------------------- |
| `[pattern]`   | optional                                                                    |
| `{pattern}`   | zero or more repetitions                                                    |
| `(pattern)`   | grouping                                                                    |
| `pat1⎮pat2`   | choice                                                                      |
| `pat1\pat2`   | difference -- elements generated by `pat1` except those generated by `pat2` |
| `'terminal'`  | terminal syntax surrounded by single quotes                                 |

<!-- Apparently, `mdbook`'s markdown can't escape the vertical bar in codeblocks in a table....
     So, we're using code point U+23AE to look like a vertical bar when it really isn't...

| `pat1|pat2`  | choice                                                                      | 
-->

Note that the terminal syntax permits C-style escape sequences e.g. `'\n'` denotes line feed (newline), and `'\r'` denotes carriage return.

Productions will be of the form:

```text
nonterm -> alt1 | ... | altn
```

## Input file representation

The input file is Unicode text where the encoding is subject to the system locale. We will often use the unqualified term *character* to refer to a Unicode code point in the input file.

## Characters

The following terms are used to denote specific Unicode character categories:

- `upper` denotes a Unicode code point categorized as an uppercase letter or titlecase letter (i.e., with General Category value Lt or Lu).

- `lower` denotes a Unicode code point categorized as a lower-case letter (i.e., with General Category value Ll).

- `alphanum` denotes either `upper` or `lower`; or a Unicode code point categorized as a modifier letter, other letter, decimal digit number, letter number, or other number (i.e., with General Category value Lt, Lu, Ll, Lm, Lo, Nd, Nl or No).

- `space` denotes a Unicode code point categorized as a separator space (i.e., with General Category value Zs), or any of the control characters `'\t'`, `'\n'`, `'\r'`, `'\f'`, or `'\v'`.

Interested readers may find details of Unicode character categories in [Section 4.5 of The Unicode Standard 15.1.0](https://www.unicode.org/versions/Unicode15.1.0/), and the [Unicode Character Database](https://unicode.org/ucd/).

## Lexical syntax

Tokens form the vocabulary of LambdaBuffers Frontend files. The classes of tokens are defined as follows.

```text
keyword         -> 'module' | 'sum' | 'prod' | 'record'
                 | 'opaque' | 'class' | 'instance' | 'import' 
                 | 'qualified' | 'as'
modulename      -> uppercamelcase
longmodulename  -> modulealias modulename
typename        -> uppercamelcase
fieldname       -> lowercamelcase\keyword
longtypename    -> modulealias typename
varname         -> lowers\keyword
punctuation     -> '<=' | ',' | '(' | ')' | '{' | '}' 
                 | ':' | ':-' | '=' | '|'
classname       -> uppercamelcase
longclassname   -> modulealias uppercamelcase
```

where

```text
uppercamelcase -> upper { alphanum }
lowercamelcase -> lower { alphanum }
modulealias    -> { uppercamelcase '.' }
lowers         -> lower { lower }
```

Input files are broken into *tokens* which use the *maximal munch* rule i.e., at each point, the next token is the longest sequence of characters that form a valid token. `space`s or line comments are ignored except as it separates tokens that would otherwise combine into a single token.

### Line comments

A *line comment* starts with the terminal `'--'` followed by zero or more printable Unicode characters stopping at the first end of line (`'\n'` or `'\r\n'`).

## Syntax of LambdaBuffers Frontend files

A LambdaBuffers Frontend file defines a module that is a collection of data types, classes, instance clauses, and derive clauses.

The overall layout of a LambdaBuffers Frontend file is:

```text
module -> 'module' longmodulename { import } { statement }
```

The file must specify the module's `longmodulename` where its `modulename` must match the LambdaBuffers Frontend file's file name not including the `.lbf` extension.
After, the file may contain a sequence of `import`s followed by a sequence of `statement`s.

### Import

Imports bring *entities* (types and classes) of other modules into scope.

```text
import     -> 'import' [ 'qualified' ] longmodulename [ 'as' longmodulename ] [ importspec ]
importspec -> '(' [ { typename ',' } typename [','] ] ')'
```

If `importspec` is omitted, then all entities specified in the module are imported; otherwise only the specified entities are imported.

### Statement

Statements define types, classes, instance clauses, and derive clauses.

```text
statement -> typedef
           | classdef
           | instanceclause
           | deriveclause
```

#### Type definitions

Types may be either sum types, product types, record types, or opaque types.

```text
typedef -> prodtypedef | sumtypedef |  recordtypedef | opaquetypedef
```

##### Product type definition

A product type definition defines a new product type.

```text
prodtypedef -> 'prod' typename { varname } '=' prod
prod        -> { typeexp }
typeexp     -> varname
             | longtypename
             | '(' prod ')'
```

Product type definitions instruct the code generator to generate a product type for the target language.

##### Sum type definition

A sum type definition defines a new sum type.

```text
sumtypedef     -> 'sum' typename { varname } '=' sum
sum            -> sumconstructor { '|' sumconstructor }
sumconstructor -> typename prod
```

Sum type definitions instruct the code generator to generate a sum type for the target language.

##### Record type definition

A record type definition defines a new record type.

```text
recordtypedef -> 'record' typename { varname } '=' record
record        -> '{' [ field { ',' field  } ] '}'
field         -> fieldname ':' prod
````

Record type definitions instruct the code generator to generate a record type for the target language.

##### Opaque type definition

An opaque type definition defines a new opaque type.

```text
opaquetypedef -> 'opaque' typename { varname }
```

Opaque type definitions must map to existing types in the target language and it's up to the Codegen module to determine how that's exactly done.

#### Class definition

A class definition introduces a new class.

```text
classdef       -> 'class' [ constraintexps '<=' ] classname { varname }
constraintexp  -> classref { varname }
                | '(' constraintexps ')'
constraintexps -> [ constraintexp { ',' constraintexp } ]
```

Class definitions communicate with the code generator the implementations that already exist (via instance clauses) or that one would like to generate (via derive clauses).

#### Instance clause

An instance clause specifies a type is an instance of a class.

```text
instanceclause -> 'instance'  constraint [ ':-' constraintexps ]
constraint     -> classref { typeexp }
```

Instance clauses do not instruct the code generator to generate code, but instead instructs the compiler (semantic checking) that the target language environment provides type class implementations for the given type (provided that the given `constraintexps` also have implementations).

#### Derive clause

Derive clauses instruct the code generator to generate code for a type so that it is an instance of a class.

```text
deriveclause -> 'derive' constraint
```

Note the code generation of a type for a class is implemented via builtin derivation rules (which developers may extend).

### Syntax reference

The summarized productions of a LambdaBuffers Frontend file is as follows.

```text
module -> 'module' longmodulename { import } { statement }

import     -> 'import' [ 'qualified' ] longmodulename [ 'as' longmodulename ] [ importspec ]
importspec -> '(' [ { typename ',' } typename [','] ] ')'

statement -> typedef
           | classdef
           | instanceclause
           | deriveclause

typedef -> prodtypedef | sumtypedef |  recordtypedef | opaquetypedef

prodtypedef -> 'prod' typename { varname } '=' prod
prod        -> { typeexp }
typeexp     -> varname
             | longtypename
             | '(' prod ')'

sumtypedef     -> 'sum' typename { varname } '=' sum
sum            -> sumconstructor { '|' sumconstructor }
sumconstructor -> typename prod

recordtypedef -> 'record' typename { varname } '=' record
record        -> '{' [ field { ',' field  } ] '}'
field         -> fieldname ':' prod

opaquetypedef -> 'opaque' typename { varname }

classdef       -> 'class' [ constraintexps '<=' ] classname { varname }
constraintexp  -> classref { varname }
                | '(' constraintexps ')'
constraintexps -> [ constraintexp { ',' constraintexp } ]

instanceclause -> 'instance'  constraint [ ':-' constraintexps ]
constraint     -> classref { typeexp }

deriveclause -> 'derive' constraint
```