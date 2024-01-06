# LambdaBuffers to Purescript

Let's take a look at how LambdaBuffers modules map into Purescript modules and how
LambdaBuffers type definitions map into Purescript type definitions.

We'll use the `lbf-prelude-to-purescript` CLI tool which is just a convenient wrapper over
the raw `lbf` CLI. We can get this tool by either loading the LambdaBuffers Nix
environment that comes packaged with all the CLI tools:

```shell
$ nix develop github:mlabs-haskell/lambda-buffers#lb
$ lbf<tab>
lbf                        lbf-plutus-to-purescript   lbf-prelude-to-purescript
lbf-plutus-to-haskell      lbf-prelude-to-haskell
```

Or we can simply just refer directly to the `lbf-prelude-to-purescript` CLI by `nix run
github:mlabs-haskell/lambda-buffers#lbf-prelude-to-purescript`.

In this chapter, we're going to use the latter option.

Let's now use `lbf-prelude-to-purescript` to process the [Document.lbf](https://github.com/mlabs-haskell/lambda-buffers/blob/main/docs/examples/Document.lbf) schema

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

```shell
$ nix run github:mlabs-haskell/lambda-buffers#lbf-prelude-to-purescript -- Document.lbf
$ find autogen/
autogen/
autogen/build.json
autogen/LambdaBuffers
autogen/LambdaBuffers/Document.purs
```

As we can see the `autogen` directory has been created that contains the generated Purescript modules.
Note the `autogen/build.json` file as it contains all the necessary dependencies the generated module needs in order to be properly compiled by `purs` compiler.

The outputted Purescript module in `autogen/LambdaBuffers/Document.hs`:

```purescript
module LambdaBuffers.Document (Author(..)
                              , Chapter(..)
                              , Document(..)
                              , Reviewer(..)
                              , RichContent(..)
                              , RichDocument(..)) where

import LambdaBuffers.Prelude as LambdaBuffers.Prelude
import Data.Generic.Rep as Data.Generic.Rep
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic


data Author = Author'Ivan  | Author'Jovan  | Author'Savo 
derive instance Data.Generic.Rep.Generic Author _
instance Data.Show.Show Author where
  show = Data.Show.Generic.genericShow

newtype Chapter a = Chapter { content :: a
                            , subChapters :: LambdaBuffers.Prelude.List (Chapter a)}
derive instance Data.Newtype.Newtype (Chapter a) _
derive instance Data.Generic.Rep.Generic (Chapter a) _
instance (Data.Show.Show a) => Data.Show.Show (Chapter a) where
  show = Data.Show.Generic.genericShow

newtype Document a = Document { author :: Author
                              , reviewers :: LambdaBuffers.Prelude.Set Reviewer
                              , content :: Chapter a}
derive instance Data.Newtype.Newtype (Document a) _
derive instance Data.Generic.Rep.Generic (Document a) _
instance (Data.Show.Show a) => Data.Show.Show (Document a) where
  show = Data.Show.Generic.genericShow

data Reviewer = Reviewer'Bob  | Reviewer'Alice 
derive instance Data.Generic.Rep.Generic Reviewer _
instance Data.Show.Show Reviewer where
  show = Data.Show.Generic.genericShow

data RichContent = RichContent'Image LambdaBuffers.Prelude.Bytes
                    | RichContent'Gif LambdaBuffers.Prelude.Bytes
                    | RichContent'Text LambdaBuffers.Prelude.Text
derive instance Data.Generic.Rep.Generic RichContent _
instance Data.Show.Show RichContent where
  show = Data.Show.Generic.genericShow

newtype RichDocument = RichDocument (Document RichContent)
derive instance Data.Newtype.Newtype RichDocument _
derive instance Data.Generic.Rep.Generic RichDocument _
instance Data.Show.Show RichDocument where
  show = Data.Show.Generic.genericShow
```

## Sum types

The types `Author`, `Reviewer`, and `RichContent` have been declared as sum types in the LambdaBuffers schema using the `sum` keyword.

As we can see, nothing too surprising here, all the `sum` types become `data`
in Purescript.

The only thing to notice is that the type name was prepended with `'` (single
quote) to the defined constructor names as to make sure they are unique.

## Product types

The type `RichDocument` have been declared as a product type in the
LambdaBuffers schema using the `prod` keyword.

They become Purescript `newtype` if they have a single type in their body, otherwise they are `data`.

Note that the constructor has the same name as the type.

## Record types

The types `Document` and `Chapter` have been declared as record types in the
LambdaBuffers schema using the `record` keyword.

They always become Purescript `newtype`, and wrapped within is a Purescript
record type with the fields named exactly like they are named in the
LambdaBuffers source module.

Also like with product types, the constructor has the same name as the type.
