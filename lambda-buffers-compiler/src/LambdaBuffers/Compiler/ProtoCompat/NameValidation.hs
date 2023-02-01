module LambdaBuffers.Compiler.ProtoCompat.NameValidation (
  varname,
  tyname,
  modulenamepart,
  constrname,
  classname,
  fieldname,
  type NameValidator,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, some)
import Text.Megaparsec.Char (alphaNumChar, lowerChar, upperChar)

type NameValidator = Parsec Void Text Text

varname :: NameValidator
varname = T.pack <$> some lowerChar

tyname :: NameValidator
tyname = do
  x <- upperChar
  xs <- some alphaNumChar
  pure . T.pack $ x : xs

modulenamepart :: NameValidator
modulenamepart = tyname

constrname :: NameValidator
constrname = tyname

classname :: NameValidator
classname = tyname

fieldname :: NameValidator
fieldname = do
  x <- lowerChar
  xs <- some alphaNumChar
  pure . T.pack $ x : xs
