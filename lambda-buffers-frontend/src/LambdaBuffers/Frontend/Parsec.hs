module LambdaBuffers.Frontend.Parsec where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero), void)
import Data.Kind (Type)
import Data.String (IsString (fromString))
import Data.Text (Text)
import LambdaBuffers.Frontend.Syntax (ConstrName (ConstrName), Constructor (Constructor), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), SourceInfo (SourceInfo), SourcePos (SourcePos), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, Sum), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName))
import Text.Parsec (ParsecT, Stream, alphaNum, char, endOfLine, eof, getPosition, label, lower, many, many1, optionMaybe, optional, runParserT, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, string, try)
import Text.Parsec.Char (upper)

type ParseState :: Type
newtype ParseState = PS
  { includeSourceInfo :: Bool
  }

type Parser :: Type -> (Type -> Type) -> Type -> Type
type Parser s m a = ParsecT s ParseState m a

parseTest' :: (Stream s IO Char, Show a) => Parser s IO a -> s -> IO ()
parseTest' p input = do
  resOrErr <- runParserT (p <* eof) (PS False) "test" input
  case resOrErr of
    Left err -> do
      print err
    Right res -> do
      print res

parseUpperCamelCase :: Stream s m Char => Parser s m Text
parseUpperCamelCase = label' "UpperCamelCase" $ fromString <$> ((:) <$> upper <*> many alphaNum)

parseModuleNamePart :: Stream s m Char => Parser s m ModuleNamePart
parseModuleNamePart = withSourceInfo . label' "module part name" $ ModuleNamePart <$> parseUpperCamelCase

parseModuleName :: Stream s m Char => Parser s m ModuleName
parseModuleName = withSourceInfo . label' "module name" $ ModuleName <$> sepBy (try parseModuleNamePart) (try $ char '.')

parseTyVarName :: Stream s m Char => Parser s m VarName
parseTyVarName = withSourceInfo . label' "type variable name" $ VarName . fromString <$> many1 lower

parseTyName :: Stream s m Char => Parser s m TyName
parseTyName = withSourceInfo . label' "type name" $ TyName <$> parseUpperCamelCase

parseModuleAlias :: Stream s m Char => Parser s m ModuleAlias
parseModuleAlias = withSourceInfo . label' "module alias" $ ModuleAlias <$> parseUpperCamelCase

parseTyRef' :: Stream s m Char => Parser s m TyRef
parseTyRef' = withSourceInfo . label' "type reference" $ do
  mayAlias <- optionMaybe (try $ parseModuleAlias <* char '.')
  TyRef mayAlias <$> parseTyName

parseTyVar :: Stream s m Char => Parser s m Ty
parseTyVar = withSourceInfo . label' "type variable" $ TyVar <$> parseTyVarName

parseTyRef :: Stream s m Char => Parser s m Ty
parseTyRef = withSourceInfo . label' "type reference" $ TyRef' <$> parseTyRef'

parseTy :: Stream s m Char => Parser s m Ty
parseTy = label' "top level type expression" $ try parseTys >>= tysToTy

parseTys :: Stream s m Char => Parser s m [Ty]
parseTys = label' "type list" $ sepEndBy parseTy' (many1 lineSpace)

parseTy' :: Stream s m Char => Parser s m Ty
parseTy' =
  label' "non-top level type expression" $
    parseTyRef
      <|> parseTyVar
      <|> ( (char '(' >> many lineSpace)
              *> (try parseTys >>= tysToTy)
              <* (many lineSpace >> char ')')
          )

tysToTy :: Stream s m Char => [Ty] -> Parser s m Ty
tysToTy tys = withSourceInfo $ case tys of
  [] -> mzero
  [ty] -> return $ const ty
  f : as -> return $ TyApp f as

parseSumBody :: Stream s m Char => Parser s m TyBody
parseSumBody = withSourceInfo . label' "sum type body" $ do
  cs <-
    sepBy
      parseSumConstructor
      (char '|' >> many1 lineSpace)
  return $ Sum cs

parseSumConstructor :: Stream s m Char => Parser s m Constructor
parseSumConstructor = withSourceInfo . label' "sum type constructor" $ Constructor <$> parseConstructorName <*> parseProduct

parseProduct :: Stream s m Char => Parser s m Product
parseProduct = do
  maySpace <- optionMaybe lineSpace
  case maySpace of
    Nothing -> withSourceInfo . label' "empty constructor" $ do
      return $ Product []
    Just _ -> withSourceInfo . label' "type product" $ do
      _ <- many lineSpace
      Product <$> parseTys

parseConstructorName :: Stream s m Char => Parser s m ConstrName
parseConstructorName = withSourceInfo . label' "sum constructor name" $ ConstrName <$> parseUpperCamelCase

parseTyDef :: Stream s m Char => Parser s m TyDef
parseTyDef = label' "type definition" $ parseSumTyDef <|> parseOpaqueTyDef

parseSumTyDef :: Stream s m Char => Parser s m TyDef
parseSumTyDef = withSourceInfo . label' "sum type definition" $ do
  _ <- string "sum"
  _ <- many1 lineSpace
  tyN <- parseTyName
  _ <- many1 lineSpace
  args <- sepEndBy parseTyArg (many1 lineSpace)
  _ <- char '='
  _ <- many1 lineSpace
  TyDef tyN args <$> parseSumBody

parseOpaqueTyDef :: Stream s m Char => Parser s m TyDef
parseOpaqueTyDef = withSourceInfo . label' "opaque type definition" $ do
  _ <- string "opaque"
  _ <- many1 lineSpace
  tyN <- parseTyName
  maySpace <- optionMaybe lineSpace
  args <- case maySpace of
    Nothing -> many lineSpace >> return []
    Just _ -> do
      _ <- many lineSpace
      sepBy parseTyArg (many1 lineSpace)
  return $ TyDef tyN args Opaque

parseTyArg :: Stream s m Char => Parser s m TyArg
parseTyArg = withSourceInfo . label' "type argument" $ do
  VarName vn _ <- parseTyVarName
  return $ TyArg vn

lineSpace :: Stream s m Char => Parser s m ()
lineSpace = label' "line space" $ void $ try $ do
  optional endOfLine
  char ' ' <|> char '\t'

lbNewLine :: Stream s m Char => Parser s m ()
lbNewLine = label' "lb new line" $ void endOfLine

parseModule :: Stream s m Char => Parser s m Module
parseModule = withSourceInfo . label' "module definition" $ do
  _ <- string "module"
  _ <- many1 lineSpace
  modName <- parseModuleName
  _ <- many lineSpace
  _ <- many1 lbNewLine
  tyDs <- sepBy parseTyDef (many1 lbNewLine)
  _ <- many space
  return $ Module modName [] tyDs

getSourcePosition :: Stream s m Char => Parser s m SourcePos
getSourcePosition = do
  pos <- getPosition
  return $ SourcePos (sourceLine pos) (sourceColumn pos)

withSourceInfo :: Stream s m Char => Parser s m (SourceInfo -> a) -> Parser s m a
withSourceInfo p = do
  pos <- getSourcePosition
  x <- p
  pos' <- getSourcePosition
  filename <- fromString . sourceName <$> getPosition
  return $ x $ SourceInfo filename pos pos'

label' :: String -> Parser s m a -> Parser s m a
label' l m = label m l
