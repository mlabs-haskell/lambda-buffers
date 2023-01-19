{-# LANGUAGE OverloadedStrings #-}

module LambdaBuffers.Gen.PP where

import Prettyprinter
import Data.Text (Text)

(</>) :: Doc a -> Doc a -> Doc a
a </> b = a <> line <> b

(<//>) :: Doc a -> Doc a -> Doc a
a <//> b = a <> line <> line <> b

(<.>) :: Doc a -> Doc a -> Doc a
x <.> y = x <> "." <> y

rBraces :: Doc a -> Doc a
rBraces d = "{" </> indent 2 d </> "}"

rRecField :: Text -> Doc a -> Doc a
rRecField l f = pretty l <> ":" <+> f

impl :: Doc a -> Text -> Doc a -> Doc a
impl cName tName body =
  "impl" <+>  cName <+> "for" <+> pretty tName <+> rBraces body

rMatchExp :: Doc a -> [Doc a] -> Doc a
rMatchExp var cases
  = "match" <+> var <+> rBraces (vcat . punctuate "," $ cases )

rCase :: Doc a -> Doc a -> Doc a
rCase pat body = pat <+> "=>" <+> body

rTuple :: [Doc a] -> Doc a
rTuple = parens . hcat . punctuate ","

rWildCase :: Doc a -> Doc a
rWildCase = rCase "_"

method2 :: Doc a -- fun name
        -> Doc a -- return type
        -> Doc a -- function body
        -> Doc a
method2 fName resTy fBody =
  "fn" <+> fName <> "(&self, other: &Self) ->" <+> resTy <+> rBraces fBody
