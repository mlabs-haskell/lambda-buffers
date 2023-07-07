# LambdaBuffers to Purescript

Let's take a look at how LambdaBuffers modules map into Purescript modules and how
LambdaBuffers type definitions map into Purescript type definitions.

Note that in this chapter we work with a 'pure' LambdaBuffers module, no
`opaque`s or type clasess, to demonstrate how pure type definition mapping
works.

We'll use the `lbf-to-purescript` CLI tool which is just a convenient wrapper over
the raw `lbf` CLI. We can get this tool by either loading the LambdaBuffers Nix
environment that comes packaged with all the CLI tools:

```shell
$ nix develop github:mlabs-purescript/lambda-buffers#lb
$ lb<tab>
lbc lbf lbg lbg-purescript lbg-haskelll lbf-to-purescript lbf-to-haskell
```

Or we can simply just refer directly to the `lbf-to-purescript` CLI by `nix run
github:mlabs-purescript/lambda-buffers#lbf-to-purescript`.

In this chapter, we're going to use the latter option.

Let's now use `lbf-to-purescript` to process the

[Document.lbf](examples/Document.lbf) schema

```purescript
module Document

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

-- ## We need a Char type that is either a letter, number or punctuation
sum Char = Letter Letter | Number Number | Punctuation Punctuation

sum Letter = A | B | C

sum Number = Num0 | Num1 | Num2

sum Punctuation = Dot | Question

-- ## String
prod String = (List Char)
```

```shell
$ nix run github:mlabs-purescript/lambda-buffers#lbf-to-purescript -- Document.lbf
$ find autogen/
autogen/
autogen/build.json
autogen/LambdaBuffers
autogen/LambdaBuffers/Document.purs
```

As we can see the `autogen` directory has been created that contains the generated Purescript modules.
Note the `autogen/build.json` file as it contains all the necessary Hackage dependencies the generated module needs in order to be properly compiled by GHC.

The outputted Purescript module in `autogen/LambdaBuffers/Document.hs`:

```purescript
module LambdaBuffers.Document (Author(..)
                              , Chapter(..)
                              , Char(..)
                              , Document(..)
                              , Gif(..)
                              , Image(..)
                              , Letter(..)
                              , List(..)
                              , Number(..)
                              , Punctuation(..)
                              , Reviewer(..)
                              , RichContent(..)
                              , RichDocument(..)
                              , String(..)) where

import Data.Generic.Rep as Data.Generic.Rep
import Data.Newtype as Data.Newtype
import Data.Show as Data.Show
import Data.Show.Generic as Data.Show.Generic


data Author = Author'Ivan  | Author'Jovan  | Author'Savo
derive instance Data.Generic.Rep.Generic Author _
instance Data.Show.Show Author where
  show = Data.Show.Generic.genericShow

newtype Chapter a = Chapter { content :: a, subChapters :: List (Chapter a)}
derive instance Data.Newtype.Newtype (Chapter a) _
derive instance Data.Generic.Rep.Generic (Chapter a) _
instance (Data.Show.Show a) => Data.Show.Show (Chapter a) where
  show = Data.Show.Generic.genericShow

data Char = Char'Letter Letter
             | Char'Number Number
             | Char'Punctuation Punctuation
derive instance Data.Generic.Rep.Generic Char _
instance Data.Show.Show Char where
  show = Data.Show.Generic.genericShow

newtype Document a = Document { author :: Author
                              , reviewers :: List Reviewer
                              , content :: Chapter a}
derive instance Data.Newtype.Newtype (Document a) _
derive instance Data.Generic.Rep.Generic (Document a) _
instance (Data.Show.Show a) => Data.Show.Show (Document a) where
  show = Data.Show.Generic.genericShow

data Gif = Gif'FunnyGif  | Gif'InspiringGif
derive instance Data.Generic.Rep.Generic Gif _
instance Data.Show.Show Gif where
  show = Data.Show.Generic.genericShow

data Image = Image'FunnyImage  | Image'BoringImage
derive instance Data.Generic.Rep.Generic Image _
instance Data.Show.Show Image where
  show = Data.Show.Generic.genericShow

data Letter = Letter'A  | Letter'B  | Letter'C
derive instance Data.Generic.Rep.Generic Letter _
instance Data.Show.Show Letter where
  show = Data.Show.Generic.genericShow

data List a = List'Nil  | List'Cons a (List a)
derive instance Data.Generic.Rep.Generic (List a) _
instance (Data.Show.Show a) => Data.Show.Show (List a) where
  show = Data.Show.Generic.genericShow

data Number = Number'Num0  | Number'Num1  | Number'Num2
derive instance Data.Generic.Rep.Generic Number _
instance Data.Show.Show Number where
  show = Data.Show.Generic.genericShow

data Punctuation = Punctuation'Dot  | Punctuation'Question
derive instance Data.Generic.Rep.Generic Punctuation _
instance Data.Show.Show Punctuation where
  show = Data.Show.Generic.genericShow

data Reviewer = Reviewer'Bob  | Reviewer'Alice
derive instance Data.Generic.Rep.Generic Reviewer _
instance Data.Show.Show Reviewer where
  show = Data.Show.Generic.genericShow

data RichContent = RichContent'Image Image String
                    | RichContent'Gif Gif String
                    | RichContent'Text String
derive instance Data.Generic.Rep.Generic RichContent _
instance Data.Show.Show RichContent where
  show = Data.Show.Generic.genericShow

newtype RichDocument = RichDocument (Document RichContent)
derive instance Data.Newtype.Newtype RichDocument _
derive instance Data.Generic.Rep.Generic RichDocument _
instance Data.Show.Show RichDocument where
  show = Data.Show.Generic.genericShow

newtype String = String (List Char)
derive instance Data.Newtype.Newtype String _
derive instance Data.Generic.Rep.Generic String _
instance Data.Show.Show String where
  show = Data.Show.Generic.genericShow
```

## Sum types

The types `Author`, `Reviewer`, `RichContent`, `Image`, `Gif`, `List`, `Char`,
`Letter`, `Number` and `Punctuation` have been declared as sum types in the
LamdaBuffers schema using the `sum` keyword.

As we can see, notihing too surprising here, all the `sum` types become `data`
in Purescript.

The only thing to notice is that the type name was prepended with `'` (single
quote) to the defined constructor names as to make sure they are unique.

## Product types

The types `RichDocument` and `String` have been declared as product types in the
LamdaBuffers schema using the `prod` keyword.

They become Purescript `newtype` if they have a single type in their body, otherwise they are `data`.

Note that the constructor has the same name as the type.

## Record types

The types `Document` and `Chapter` have been declared as record types in the
LamdaBuffers schema using the `record` keyword.

They always become Purescript `newtype`, and wrapped within is a Purescript
record type with the fields named exactly like they are named in the
LambdaBuffers source module.

Also like with product types, the constructor has the same name as the type.
