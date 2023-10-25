# LambdaBuffers to Haskell

Let's take a look at how LambdaBuffers modules map into Haskell modules and how
LambdaBuffers type definitions map into Haskell type definitions.

We'll use the `lbf-prelude-to-haskell` CLI tool which is just a convenient wrapper over
the raw `lbf` CLI. We can get this tool by either loading the LambdaBuffers Nix
environment that comes packaged with all the CLI tools:

```shell
$ nix develop github:mlabs-haskell/lambda-buffers#lb
$ lbf<tab>
lbf                        lbf-plutus-to-purescript   lbf-prelude-to-purescript
lbf-plutus-to-haskell      lbf-prelude-to-haskell
```

Or we can simply just refer directly to the `lbf-prelude-to-haskell` CLI by `nix run
github:mlabs-haskell/lambda-buffers#lbf-prelude-to-haskell`.

In this chapter, we're going to use the latter option.

Let's now use `lbf-prelude-to-haskell` to process the [Document.lbf](examples/Document.lbf) schema.

```purescript
module Document

-- We need the opaque Char type
import Prelude (Char)

-- Author
sum Author = Ivan | Jovan | Savo

-- Reviewer
sum Reviewer = Bob | Alice

-- Document
record Document a = {
  author : Author,
  reviewers : List Reviewer,
  content : Chapter a
 }

-- Chapter
record Chapter a = {
  content : a,
  subChapters : List (Chapter a)
 }

-- Some actual content
sum RichContent = Image Image String | Gif Gif String | Text String

sum Image = FunnyImage | BoringImage

sum Gif = FunnyGif | InspiringGif

-- Rich document

prod RichDocument = (Document RichContent)

-- # Some basic types

-- ## We need a list type
sum List a = Nil | Cons a (List a)

-- ## String
prod String = (List Char)
```

```shell
$ nix run github:mlabs-haskell/lambda-buffers#lbf-prelude-to-haskell -- Document.lbf
$ find autogen/
autogen/
autogen/LambdaBuffers
autogen/LambdaBuffers/Document.hs
autogen/build.json
```

As we can see the `autogen` directory has been created that contains the generated Haskell modules.
Note the `autogen/build.json` file as it contains all the necessary Hackage dependencies the generated module needs in order to be properly compiled by GHC.

The outputted Haskell module in `autogen/LambdaBuffers/Document.hs`:

```haskell
module LambdaBuffers.Document (Author(..)
                              , Chapter(..)
                              , Document(..)
                              , Gif(..)
                              , Image(..)
                              , List(..)
                              , Reviewer(..)
                              , RichContent(..)
                              , RichDocument(..)
                              , String(..)) where

import qualified LambdaBuffers.Prelude
import qualified Prelude


data Author = Author'Ivan  | Author'Jovan  | Author'Savo  deriving Prelude.Show

data Chapter a = Chapter { chapter'content :: a
                         , chapter'subChapters :: List (Chapter a)} deriving Prelude.Show

data Document a = Document { document'author :: Author
                           , document'reviewers :: List Reviewer
                           , document'content :: Chapter a} deriving Prelude.Show

data Gif = Gif'FunnyGif  | Gif'InspiringGif  deriving Prelude.Show

data Image = Image'FunnyImage  | Image'BoringImage  deriving Prelude.Show

data List a = List'Nil  | List'Cons a (List a) deriving Prelude.Show

data Reviewer = Reviewer'Bob  | Reviewer'Alice  deriving Prelude.Show

data RichContent = RichContent'Image Image String
                    | RichContent'Gif Gif String
                    | RichContent'Text String deriving Prelude.Show

newtype RichDocument = RichDocument (Document RichContent) deriving Prelude.Show

newtype String = String (List LambdaBuffers.Prelude.Char) deriving Prelude.Show
```

We can compile the code with the following commands.
Note the dev shell `dev-prelude-haskell` as it includes the `LambdaBuffers.Prelude` dependency.

```shell
$ nix develop github:mlabs-haskell/lambda-buffers#dev-prelude-haskell
$ ghc autogen/LambdaBuffers/Document.hs
[1 of 1] Compiling LambdaBuffers.Document ( autogen/LambdaBuffers/Document.hs, autogen/LambdaBuffers/Document.o )
```

## Sum types

The types `Author`, `Reviewer`, `RichContent`, `Image`, `Gif`, and `List` have been declared as sum types in the LamdaBuffers schema using the `sum` keyword.

As we can see, nothing too surprising here, all the `sum` types become `data`
in Haskell.

The only thing to notice is that the type name was prepended with `'` (single
quote) to the defined constructor names as to make sure they are unique.

## Product types

The types `RichDocument` and `String` have been declared as product types in the
LamdaBuffers schema using the `prod` keyword.

They become Haskell `newtype` if they have a single type in their body, otherwise they are `data`.

Note that the constructor has the same name as the type.

## Record types

The types `Document` and `Chapter` have been declared as record types in the
LamdaBuffers schema using the `record` keyword.

Like with product types, they become Haskell `newtype` if they have a single
type in their body, otherwise they are `data`.

Also like with product types, the constructor has the same name as the type.

The field names, similar to sum constructor names, are prepended with the
lowercased named of the type with a single quote (`'`) to maintain uniqueness.
