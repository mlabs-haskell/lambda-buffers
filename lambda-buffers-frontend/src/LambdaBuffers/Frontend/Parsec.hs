module LambdaBuffers.Frontend.Parsec (parseModule, parseImport, runParser) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero), void)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import Data.Text (Text)
import LambdaBuffers.Frontend.Syntax (ConstrName (ConstrName), Constructor (Constructor), Import (Import), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), SourceInfo (SourceInfo), SourcePos (SourcePos), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, Sum), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName))
import Text.Parsec (ParseError, ParsecT, SourceName, Stream, alphaNum, char, endOfLine, eof, getPosition, label, lower, many, many1, optionMaybe, optional, runParserT, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, string, try)
import Text.Parsec.Char (upper)

type Parser :: Type -> (Type -> Type) -> Type -> Type
type Parser s m a = ParsecT s () m a

runParser :: (Stream s IO Char) => Parser s IO a -> SourceName -> s -> IO (Either ParseError a)
runParser p = runParserT (p <* eof) ()

parseUpperCamelCase :: Stream s m Char => Parser s m Text
parseUpperCamelCase = label' "UpperCamelCase" $ fromString <$> ((:) <$> upper <*> many alphaNum)

parseModuleNamePart :: Stream s m Char => Parser s m (ModuleNamePart SourceInfo)
parseModuleNamePart = withSourceInfo . label' "module part name" $ ModuleNamePart <$> parseUpperCamelCase

parseModuleName :: Stream s m Char => Parser s m (ModuleName SourceInfo)
parseModuleName = withSourceInfo . label' "module name" $ ModuleName <$> sepBy (try parseModuleNamePart) (try $ char '.')

parseTyVarName :: Stream s m Char => Parser s m (VarName SourceInfo)
parseTyVarName = withSourceInfo . label' "type variable name" $ VarName . fromString <$> many1 lower

parseTyName :: Stream s m Char => Parser s m (TyName SourceInfo)
parseTyName = withSourceInfo . label' "type name" $ TyName <$> parseUpperCamelCase

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
parseTys = label' "type list" $ sepEndBy parseTy' lineSpaces1

parseTy' :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTy' =
  label' "type expression" $
    parseTyRef
      <|> parseTyVar
      <|> ( (char '(' >> lineSpaces)
              *> (try parseTys >>= tysToTy)
              <* (lineSpaces >> char ')')
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
      (char '|' >> lineSpaces1)
  return $ Sum cs

parseSumConstructor :: Stream s m Char => Parser s m (Constructor SourceInfo)
parseSumConstructor = withSourceInfo . label' "sum type constructor" $ Constructor <$> parseConstructorName <*> parseProduct

parseProduct :: Stream s m Char => Parser s m (Product SourceInfo)
parseProduct = do
  maySpace <- optionMaybe lineSpace
  case maySpace of
    Nothing -> withSourceInfo . label' "empty constructor" $ do
      return $ Product []
    Just _ -> withSourceInfo . label' "type product" $ do
      _ <- lineSpaces
      Product <$> parseTys

parseConstructorName :: Stream s m Char => Parser s m (ConstrName SourceInfo)
parseConstructorName = withSourceInfo . label' "sum constructor name" $ ConstrName <$> parseUpperCamelCase

parseTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseTyDef = label' "type definition" $ parseSumTyDef <|> parseOpaqueTyDef

parseSumTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseSumTyDef = withSourceInfo . label' "sum type definition" $ do
  _ <- string "sum"
  _ <- lineSpaces1
  tyN <- parseTyName
  _ <- lineSpaces1
  args <- sepEndBy parseTyArg lineSpaces1
  _ <- char '='
  _ <- lineSpaces1
  TyDef tyN args <$> parseSumBody

parseOpaqueTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseOpaqueTyDef = withSourceInfo . label' "opaque type definition" $ do
  _ <- string "opaque"
  _ <- lineSpaces1
  tyN <- parseTyName
  maySpace <- optionMaybe lineSpace
  args <- case maySpace of
    Nothing -> lineSpaces >> return []
    Just _ -> do
      _ <- lineSpaces
      sepBy parseTyArg lineSpaces1
  return $ TyDef tyN args Opaque

parseTyArg :: Stream s m Char => Parser s m (TyArg SourceInfo)
parseTyArg = withSourceInfo . label' "type argument" $ do
  VarName vn _ <- parseTyVarName
  return $ TyArg vn

parseModule :: Stream s m Char => Parser s m (Module SourceInfo)
parseModule = withSourceInfo . label' "module definition" $ do
  _ <- string "module"
  _ <- lineSpaces1
  modName <- parseModuleName
  _ <- lineSpaces
  _ <- many1 lbNewLine
  imports <- sepEndBy parseImport (many1 lbNewLine)
  tyDs <- sepBy parseTyDef (many1 lbNewLine)
  _ <- many space
  return $ Module modName imports tyDs

parseImport :: Stream s m Char => Parser s m (Import SourceInfo)
parseImport = withSourceInfo . label' "import statement" $ do
  _ <- string "import" >> lineSpaces1
  isQual <- isJust <$> optionMaybe (string "qualified" >> lineSpaces1)
  modName <- parseModuleName
  may <-
    optionMaybe
      ( do
          lineSpace
          mayModAlias <- optionMaybe (lineSpaces >> string "as" >> lineSpaces1 *> parseModuleAliasInImport)
          mayTyNs <-
            optionMaybe
              ( do
                  lineSpaces >> char '(' >> lineSpaces
                  tyNs <- sepEndBy parseTyName (char ',' >> lineSpaces)
                  _ <- char ')'
                  return tyNs
              )
          return (mayModAlias, mayTyNs)
      )
  case may of
    Nothing ->
      return $
        Import
          isQual
          modName
          Nothing
          Nothing
    Just (mayModAlias, mayTyNs) ->
      return $
        Import
          isQual
          modName
          mayTyNs
          mayModAlias

lbNewLine :: Stream s m Char => Parser s m ()
lbNewLine = label' "lb new line" $ void endOfLine

lineSpace :: Stream s m Char => Parser s m ()
lineSpace = label' "line space" $ void $ try $ do
  optional endOfLine
  char ' ' <|> char '\t'

lineSpaces1 :: Stream s m Char => Parser s m ()
lineSpaces1 = void $ try $ many1 lineSpace

lineSpaces :: Stream s m Char => Parser s m ()
lineSpaces = void $ try $ many lineSpace

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
