module LambdaBuffers.Compiler.NamingCheck (pModuleNamePart, pVarName, pTyName, pConstrName, pFieldName, pClassName, checkModuleName, checkTyName, checkVarName, checkConstrName, checkClassName, checkFieldName) where

import Control.Lens ((.~), (^.))
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Field (HasField)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Proto.Compiler (ClassName, ConstrName, FieldName, Module, NamingError, NamingError'NameType (NamingError'NAME_TYPE_CLASS, NamingError'NAME_TYPE_CONSTR, NamingError'NAME_TYPE_FIELD, NamingError'NAME_TYPE_MODULE, NamingError'NAME_TYPE_TYPE, NamingError'NAME_TYPE_VAR), SourceInfo, TyName, VarName)
import Proto.Compiler_Fields (moduleName, name, nameType, parts, sourceInfo)
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
  ( MonadError NamingError m
  , HasField a "sourceInfo" SourceInfo
  , HasField a "name" Text
  ) =>
  Parser Text m Text ->
  NamingError'NameType ->
  a ->
  m ()
validateP p nt i = do
  resOrErr <- runParserT p () "" (i ^. name)
  case resOrErr of
    Left _ ->
      throwError $
        defMessage
          & nameType .~ nt
          & sourceInfo .~ (i ^. sourceInfo)
    Right _ -> return ()

checkModuleName :: MonadError NamingError m => Module -> m ()
checkModuleName m = for_ (m ^. (moduleName . parts)) (validateP pModuleNamePart NamingError'NAME_TYPE_MODULE)

checkTyName :: MonadError NamingError m => TyName -> m ()
checkTyName = validateP pTyName NamingError'NAME_TYPE_TYPE

checkVarName :: MonadError NamingError m => VarName -> m ()
checkVarName = validateP pVarName NamingError'NAME_TYPE_VAR

checkConstrName :: MonadError NamingError m => ConstrName -> m ()
checkConstrName = validateP pConstrName NamingError'NAME_TYPE_CONSTR

checkFieldName :: MonadError NamingError m => FieldName -> m ()
checkFieldName = validateP pFieldName NamingError'NAME_TYPE_FIELD

checkClassName :: MonadError NamingError m => ClassName -> m ()
checkClassName = validateP pClassName NamingError'NAME_TYPE_CLASS
