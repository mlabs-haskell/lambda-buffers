# LambdaBuffers to Rust

Let's take a look at how LambdaBuffers modules map into Rust modules and how
LambdaBuffers type definitions map into Rust type definitions.

We'll use the `lbf-prelude-to-rust` CLI tool which is just a convenient wrapper over
the raw `lbf` CLI. We can get this tool by either loading the LambdaBuffers Nix
environment that comes packaged with all the CLI tools:

```shell
$ nix develop github:mlabs-haskell/lambda-buffers#lb
$ lbf<tab>
lbf                        lbf-plutus-to-purescript   lbf-prelude-to-haskell     lbf-prelude-to-rust
lbf-plutus-to-haskell      lbf-plutus-to-rust         lbf-prelude-to-purescript  
```

Or we can simply just refer directly to the `lbf-prelude-to-rust` CLI by `nix run
github:mlabs-haskell/lambda-buffers#lbf-prelude-to-rust`.

In this chapter, we're going to use the latter option.

Let's now use `lbf-prelude-to-rust` to process the [Document.lbf](https://github.com/mlabs-haskell/lambda-buffers/tree/main/docs/examples/Document.lbf) schema.

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
$ nix run github:mlabs-haskell/lambda-buffers#lbf-prelude-to-rust -- Document.lbf
$ find autogen/
autogen/
autogen/LambdaBuffers
autogen/LambdaBuffers/Document.hs
autogen/build.json
```

As we can see the `autogen` directory has been created that contains the generated Rust modules.
Note the `autogen/build.json` file as it contains all the necessary Cargo dependencies the generated module needs in order to be properly compiled by Rust.

The outputted Rust module in `autogen/document.rs`:

```rust
#![no_implicit_prelude]
#![allow(warnings)]
extern crate lbf_prelude;
extern crate std;


#[derive(std::fmt::Debug, std::clone::Clone)]
pub enum Author{Ivan, Jovan, Savo}

#[derive(std::fmt::Debug, std::clone::Clone)]
pub struct Chapter<A>{pub content: A,
                     pub sub_chapters: std::boxed::Box<lbf_prelude::prelude::List<Chapter<A>>>}

#[derive(std::fmt::Debug, std::clone::Clone)]
pub struct Document<A>{pub author: Author,
                      pub reviewers: lbf_prelude::prelude::Set<Reviewer>,
                      pub content: Chapter<A>}

#[derive(std::fmt::Debug, std::clone::Clone)]
pub enum Reviewer{Bob, Alice}

#[derive(std::fmt::Debug, std::clone::Clone)]
pub enum RichContent{Image(lbf_prelude::prelude::Bytes),
                    Gif(lbf_prelude::prelude::Bytes),
                    Text(lbf_prelude::prelude::Text)}

#[derive(std::fmt::Debug, std::clone::Clone)]
pub struct RichDocument(pub Document<RichContent>);

```

## Sum types

The types `Author`, `Reviewer`, and `RichContent` have been declared as sum types in the LambdaBuffers schema using the `sum` keyword.

As we can see, nothing too surprising here, all the `sum` types become `enum`
in Rust.

## Product types

The type `RichDocument` have been declared as a product type in the
LambdaBuffers schema using the `prod` keyword.

They become Rust tuple `struct` (or named tuple)

## Record types

The types `Document` and `Chapter` have been declared as record types in the
LambdaBuffers schema using the `record` keyword.

Like with product types, they become Rust `struct` with named fields.

All types and their fields are public, allowing to manipulate them without accessors.
