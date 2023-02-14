module LambdaBuffers.Compiler.NamingCheck (pModuleNamePart, pVarName, pTyName, pConstrName, pFieldName, pClassName, checkModuleNamePart, checkTyName, checkVarName, checkConstrName, checkClassName, checkFieldName) where

import Control.Lens (ASetter, (.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Function ((&))
import Data.Kind (Type)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Field (HasField)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Proto.Compiler (ClassName, ConstrName, FieldName, ModuleNamePart, NamingError, TyName, VarName)
import Proto.Compiler_Fields (classNameErr, constrNameErr, fieldNameErr, moduleNameErr, name, tyNameErr, varNameErr)
import Text.Parsec (ParsecT, Stream, alphaNum, label, lower, many, many1, runParserT)
import Text.Parsec.Char (upper)

type Parser :: Type -> (Type -> Type) -> Type -> Type
type Parser s m a = ParsecT s () m a

pUpperCamelCase :: Stream s m Char => Parser s m Text
pUpperCamelCase = label' "UpperCamelCase" $ fromString <$> ((:) <$> upper <*> many alphaNum)

pLowerCamelCase :: Stream s m Char => Parser s m Text
pLowerCamelCase = label' "lowerCamelCase" $ fromString <$> ((:) <$> lower <*> many alphaNum)

pModuleNamePart :: Stream s m Char => Parser s m Text
pModuleNamePart = label' "module part name" pUpperCamelCase

pVarName :: Stream s m Char => Parser s m Text
pVarName = label' "type variable name" $ fromString <$> many1 lower

pTyName :: Stream s m Char => Parser s m Text
pTyName = label' "type name" pUpperCamelCase

pConstrName :: Stream s m Char => Parser s m Text
pConstrName = label' "sum body constructor name" pUpperCamelCase

pFieldName :: Stream s m Char => Parser s m Text
pFieldName = label' "record body field name" pLowerCamelCase

pClassName :: Stream s m Char => Parser s m Text
pClassName = label' "type class name" pUpperCamelCase

label' :: String -> Parser s m a -> Parser s m a
label' l m = label m l

validateP ::
  (HasField i "name" Text, MonadError NamingError m, Message a1) =>
  Parser Text m Text ->
  ASetter a1 NamingError a3 i ->
  i ->
  m ()
validateP p f i = do
  resOrErr <- runParserT p () "" (i ^. name)
  case resOrErr of
    Left _ -> throwError $ defMessage & f .~ i
    Right _ -> return ()

checkModuleNamePart :: MonadError NamingError m => ModuleNamePart -> m ()
checkModuleNamePart = validateP pModuleNamePart moduleNameErr

checkTyName :: MonadError NamingError m => TyName -> m ()
checkTyName = validateP pTyName tyNameErr

checkVarName :: MonadError NamingError m => VarName -> m ()
checkVarName = validateP pVarName varNameErr

checkConstrName :: MonadError NamingError m => ConstrName -> m ()
checkConstrName = validateP pConstrName constrNameErr

checkFieldName :: MonadError NamingError m => FieldName -> m ()
checkFieldName = validateP pFieldName fieldNameErr

checkClassName :: MonadError NamingError m => ClassName -> m ()
checkClassName = validateP pClassName classNameErr
