module LambdaBuffers.Frontend.Parsec (parseModule, parseImport, runParser, parseTyInner, parseTyTopLevel, parseRecord, parseProduct, parseSum, parseConstraint, parseInstanceBody, parseInstanceClause, parseDerive) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero), void)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.String (IsString (fromString))
import LambdaBuffers.Compiler.NamingCheck (pClassName, pConstrName, pFieldName, pModuleNamePart, pTyName)
import LambdaBuffers.Frontend.Syntax (ClassName (ClassName), ClassRef (ClassRef), ConstrName (ConstrName), Constraint (Constraint), Constructor (Constructor), Derive (Derive), Field (Field), FieldName (FieldName), Import (Import), InstanceClause (InstanceClause), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Product (Product), Record (Record), SourceInfo (SourceInfo), SourcePos (SourcePos), Sum (Sum), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, ProductBody, RecordBody, SumBody), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName), kwDerive, kwInstance, kwTyDefOpaque, kwTyDefProduct, kwTyDefRecord, kwTyDefSum)
import Text.Parsec (ParseError, ParsecT, SourceName, Stream, between, char, endOfLine, eof, getPosition, label, lower, many, many1, optionMaybe, optional, runParserT, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, string, try)

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

parseClassName :: Stream s m Char => Parser s m (ClassName SourceInfo)
parseClassName = withSourceInfo . label' "class name" $ ClassName <$> pClassName

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
parseTyVar = label' "type variable" $ TyVar <$> parseTyVarName

parseTyRef :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTyRef = withSourceInfo . label' "type reference" $ TyRef' <$> parseTyRef'

{- | Inner type expression.
 Valid examples:

 a | (a) | ((a)) | ( a ) | (( ( a)))
 Int | (Int) | ((Int)) | (  Int  ) | (( ( Int)))
 (Maybe a) | ((Maybe) (a)) | ((Maybe) a) | (Maybe (a))

 Invalid examples:
 Maybe a
-}
parseTyInner :: forall s m. Stream s m Char => Parser s m (Ty SourceInfo)
parseTyInner = label' "inner type expression" parseSexp

parseTyTopLevel :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTyTopLevel = label' "top level type expression" $ parseTys >>= tysToTy

-- | Sexp :- var | TyRef | (Sexp)
parseSexp :: forall s m. Stream s m Char => Parser s m (Ty SourceInfo)
parseSexp = label' "s-expression" $ between parseLineSpaces parseLineSpaces (parseSexpList <|> parseSexpAtom)

parseSexpAtom :: forall s m. Stream s m Char => Parser s m (Ty SourceInfo)
parseSexpAtom = try parseTyRef <|> try parseTyVar

parseTys :: forall s m. Stream s m Char => Parser s m [Ty SourceInfo]
parseTys = many parseSexp

parseSexpList :: forall s m. Stream s m Char => Parser s m (Ty SourceInfo)
parseSexpList = between (char '(') (char ')') (parseTys >>= tysToTy)

tysToTy :: Stream s m Char => [Ty SourceInfo] -> Parser s m (Ty SourceInfo)
tysToTy tys = withSourceInfo $ case tys of
  [] -> mzero
  [ty] -> return $ const ty
  f : as -> return $ TyApp f as

parseSum :: Stream s m Char => Parser s m (Sum SourceInfo)
parseSum = withSourceInfo . label' "sum type expression" $ do
  cs <-
    sepBy
      parseSumConstructor
      (parseLineSpaces >> char '|' >> parseLineSpaces)
  return $ Sum cs

parseSumConstructor :: Stream s m Char => Parser s m (Constructor SourceInfo)
parseSumConstructor = withSourceInfo . label' "sum type constructor" $ Constructor <$> parseConstructorName <*> (parseLineSpaces >> parseProduct)

parseProduct :: Stream s m Char => Parser s m (Product SourceInfo)
parseProduct = withSourceInfo . label' "product type expression" $ Product <$> parseTys

parseRecord :: Stream s m Char => Parser s m (Record SourceInfo)
parseRecord = withSourceInfo . label' "record type expression" $ do
  fields <-
    between
      (char '{' >> parseLineSpaces)
      (parseLineSpaces >> char '}')
      $ sepBy parseField (parseLineSpaces >> char ',' >> parseLineSpaces)
  return $ Record fields

parseField :: Stream s m Char => Parser s m (Field SourceInfo)
parseField = withSourceInfo . label' "record field" $ do
  fn <- parseFieldName
  parseLineSpaces1
  _ <- char ':'
  parseLineSpaces1
  Field fn <$> parseTyTopLevel

parseFieldName :: Stream s m Char => Parser s m (FieldName SourceInfo)
parseFieldName = withSourceInfo . label' "record field name" $ FieldName <$> pFieldName

parseConstructorName :: Stream s m Char => Parser s m (ConstrName SourceInfo)
parseConstructorName = withSourceInfo . label' "sum constructor name" $ ConstrName <$> pConstrName

parseTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseTyDef = label' "type definition" $ parseSumTyDef <|> parseProdTyDef <|> parseRecordTyDef <|> parseOpaqueTyDef

parseSumTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseSumTyDef = parseTyDef' kwTyDefSum (SumBody <$> parseSum)

parseProdTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseProdTyDef = parseTyDef' kwTyDefProduct (ProductBody <$> parseProduct)

parseRecordTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseRecordTyDef = parseTyDef' kwTyDefRecord (RecordBody <$> parseRecord)

parseTyDef' :: Stream s m Char => String -> Parser s m (TyBody SourceInfo) -> Parser s m (TyDef SourceInfo)
parseTyDef' kw parseBody = withSourceInfo . label' (kw <> " type definition") $ do
  _ <- string kw
  _ <- parseLineSpaces1
  tyN <- parseTyName
  _ <- parseLineSpaces1
  args <- sepEndBy parseTyArg parseLineSpaces1
  _ <- char '='
  _ <- parseLineSpaces1
  TyDef tyN args <$> parseBody

parseOpaqueTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseOpaqueTyDef = withSourceInfo . label' "opaque type definition" $ do
  _ <- string kwTyDefOpaque
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

parseClassRef :: Stream s m Char => Parser s m (ClassRef SourceInfo)
parseClassRef = withSourceInfo . label' "class reference" $ do
  mayAlias <- optionMaybe parseModuleAliasInRef
  ClassRef mayAlias <$> parseClassName

parseConstraint :: Stream s m Char => Parser s m (Constraint SourceInfo)
parseConstraint = withSourceInfo . label' "constraint" $ Constraint <$> parseClassRef <*> parseTys

parseDerive :: Stream s m Char => Parser s m (Derive SourceInfo)
parseDerive = string kwDerive >> parseLineSpaces >> Derive <$> parseConstraint

parseInstanceClause :: Stream s m Char => Parser s m (InstanceClause SourceInfo)
parseInstanceClause = withSourceInfo . label' "instance clause" $ do
  _ <- string kwInstance
  clauseHead <- between parseLineSpaces parseLineSpaces parseConstraint
  _ <- string ":-"
  InstanceClause clauseHead <$> parseInstanceBody

parseInstanceBody :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseInstanceBody = parseConstraints

-- | Constraints sexp.
parseConstraintSexp :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraintSexp = between parseLineSpaces parseLineSpaces (parseConstraintList <|> parseConstraintAtom)

parseConstraintAtom :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraintAtom = pure <$> parseConstraint

parseConstraintList :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraintList = between (char '(') (char ')') parseConstraints

parseConstraints :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraints = concat <$> sepBy parseConstraintSexp (char ',')

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
