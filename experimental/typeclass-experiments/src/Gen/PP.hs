{-# LANGUAGE OverloadedStrings #-}

module Gen.PP where

import Prettyprinter
import Data.Text (Text)

(</>) :: Doc a -> Doc a -> Doc a
a </> b = a <> line <> b

rBraces :: Doc a -> Doc a
rBraces d = "{" </> indent 2 d </> "}"

rRecField :: Text -> Doc a -> Doc a
rRecField l f = pretty l <> ":" <+> f
