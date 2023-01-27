module LambdaBuffers.Frontend.Parsec (parseModule, parseImport, runParser) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero), void)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import LambdaBuffers.Compiler.NamingCheck (pConstrName, pModuleNamePart, pTyName)
import LambdaBuffers.Frontend.Syntax (ConstrName (ConstrName), Constructor (Constructor), Import (Import), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), SourceInfo (SourceInfo), SourcePos (SourcePos), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, Sum), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName))
import Text.Parsec (ParseError, ParsecT, SourceName, Stream, char, endOfLine, eof, getPosition, label, lower, many, many1, optionMaybe, optional, runParserT, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, string, try)

type Parser :: Type -> (Type -> Type) -> Type -> Type
type Parser s m a = ParsecT s () m a

runParser :: (Stream s IO Char) => Parser s IO a -> SourceName -> s -> IO (Either ParseError a)
runParser p = runParserT (p <* eof) ()

parseModuleNamePart :: Stream s m Char => Parser s m (ModuleNamePart SourceInfo)
parseModuleNamePart = withSourceInfo . label' "module part name" $ ModuleNamePart <$> pModuleNamePart

parseModuleName :: Stream s m Char => Parser s m (ModuleName SourceInfo)
parseModuleName = withSourceInfo . label' "module name" $ ModuleName <$> sepBy (try parseModuleNamePart) (try $ char '.')

parseTyVarName :: Stream s m Char => Parser s m (VarName SourceInfo)
parseTyVarName = withSourceInfo . label' "type variable name" $ VarName . fromString <$> many1 lower

parseTyName :: Stream s m Char => Parser s m (TyName SourceInfo)
parseTyName = withSourceInfo . label' "type name" $ TyName <$> pTyName

parseModuleAliasInRef :: Stream s m Char => Parser s m (ModuleAlias SourceInfo)
parseModuleAliasInRef =
  withSourceInfo . label' "module alias in type reference" $
    ModuleAlias <$> do
      ps <- many1 (try (parseModuleNamePart <* char '.'))
      withSourceInfo . return $ ModuleName ps

parseModuleAliasInImport :: Stream s m Char => Parser s m (ModuleAlias SourceInfo)
parseModuleAliasInImport = withSourceInfo . label' "module alias in module import" $ ModuleAlias <$> parseModuleName

parseTyRef' :: Stream s m Char => Parser s m (TyRef SourceInfo)
parseTyRef' = withSourceInfo . label' "type reference" $ do
  mayAlias <- optionMaybe parseModuleAliasInRef
  TyRef mayAlias <$> parseTyName

parseTyVar :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTyVar = withSourceInfo . label' "type variable" $ TyVar <$> parseTyVarName

parseTyRef :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTyRef = withSourceInfo . label' "type reference" $ TyRef' <$> parseTyRef'

parseTys :: Stream s m Char => Parser s m [Ty SourceInfo]
parseTys = label' "type list" $ sepEndBy parseTy' parseLineSpaces1

parseTy' :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTy' =
  label' "type expression" $
    parseTyRef
      <|> parseTyVar
      <|> ( (char '(' >> parseLineSpaces)
              *> (try parseTys >>= tysToTy)
              <* (parseLineSpaces >> char ')')
          )

tysToTy :: Stream s m Char => [Ty SourceInfo] -> Parser s m (Ty SourceInfo)
tysToTy tys = withSourceInfo $ case tys of
  [] -> mzero
  [ty] -> return $ const ty
  f : as -> return $ TyApp f as

parseSumBody :: Stream s m Char => Parser s m (TyBody SourceInfo)
parseSumBody = withSourceInfo . label' "sum type body" $ do
  cs <-
    sepBy
      parseSumConstructor
      (char '|' >> parseLineSpaces1)
  return $ Sum cs

parseSumConstructor :: Stream s m Char => Parser s m (Constructor SourceInfo)
parseSumConstructor = withSourceInfo . label' "sum type constructor" $ Constructor <$> parseConstructorName <*> parseProduct

parseProduct :: Stream s m Char => Parser s m (Product SourceInfo)
parseProduct = do
  maySpace <- optionMaybe parseLineSpace
  case maySpace of
    Nothing -> withSourceInfo . label' "empty constructor" $ do
      return $ Product []
    Just _ -> withSourceInfo . label' "type product" $ do
      _ <- parseLineSpaces
      Product <$> parseTys

parseConstructorName :: Stream s m Char => Parser s m (ConstrName SourceInfo)
parseConstructorName = withSourceInfo . label' "sum constructor name" $ ConstrName <$> pConstrName

parseTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseTyDef = label' "type definition" $ parseSumTyDef <|> parseOpaqueTyDef

parseSumTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseSumTyDef = withSourceInfo . label' "sum type definition" $ do
  _ <- string "sum"
  _ <- parseLineSpaces1
  tyN <- parseTyName
  _ <- parseLineSpaces1
  args <- sepEndBy parseTyArg parseLineSpaces1
  _ <- char '='
  _ <- parseLineSpaces1
  TyDef tyN args <$> parseSumBody

parseOpaqueTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseOpaqueTyDef = withSourceInfo . label' "opaque type definition" $ do
  _ <- string "opaque"
  _ <- parseLineSpaces1
  tyN <- parseTyName
  maySpace <- optionMaybe parseLineSpace
  args <- case maySpace of
    Nothing -> parseLineSpaces >> return []
    Just _ -> do
      _ <- parseLineSpaces
      sepBy parseTyArg parseLineSpaces1
  return $ TyDef tyN args Opaque

parseTyArg :: Stream s m Char => Parser s m (TyArg SourceInfo)
parseTyArg = withSourceInfo . label' "type argument" $ do
  VarName vn _ <- parseTyVarName
  return $ TyArg vn

parseModule :: Stream s m Char => Parser s m (Module SourceInfo)
parseModule = withSourceInfo . label' "module definition" $ do
  _ <- string "module"
  _ <- parseLineSpaces1
  modName <- parseModuleName
  _ <- parseLineSpaces
  _ <- many1 parseNewLine
  imports <- sepEndBy parseImport (many1 parseNewLine)
  tyDs <- sepEndBy parseTyDef (many1 parseNewLine)
  _ <- many space
  return $ Module modName imports tyDs

parseImport :: Stream s m Char => Parser s m (Import SourceInfo)
parseImport = withSourceInfo . label' "import statement" $ do
  _ <- string "import"
  _ <- parseLineSpaces1
  isQual <- isJust <$> optionMaybe (string "qualified" >> parseLineSpaces1)
  modName <- parseModuleName
  may <-
    optionMaybe
      ( do
          mayModAlias <- optionMaybe (try $ parseLineSpaces1 >> string "as" >> parseLineSpaces1 *> parseModuleAliasInImport)
          mayTyNs <-
            optionMaybe
              ( try $ do
                  parseLineSpaces1 >> char '(' >> parseLineSpaces
                  tyNs <- sepEndBy parseTyName (char ',' >> parseLineSpaces)
                  _ <- try parseLineSpaces >> char ')'
                  return tyNs
              )
          _ <- try parseLineSpaces
          return (mayModAlias, mayTyNs)
      )
  case may of
    Nothing -> return $ Import isQual modName Nothing Nothing
    Just (mayModAlias, mayTyNs) -> return $ Import isQual modName mayTyNs mayModAlias

parseNewLine :: Stream s m Char => Parser s m ()
parseNewLine = label' "lb new line" $ void endOfLine

parseLineSpace :: Stream s m Char => Parser s m ()
parseLineSpace = label' "line space" $ void $ try $ do
  optional endOfLine
  char ' ' <|> char '\t'

parseLineSpaces1 :: Stream s m Char => Parser s m ()
parseLineSpaces1 = void $ try $ many1 parseLineSpace

parseLineSpaces :: Stream s m Char => Parser s m ()
parseLineSpaces = void $ try $ many parseLineSpace

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
