module LambdaBuffers.Frontend.Parsec (
  parseModule,
  parseImport,
  runParser,
  parseTyInner,
  parseTyTopLevel,
  parseRecord,
  parseProduct,
  parseSum,
  parseConstraint,
  parseInstanceBody,
  parseInstanceClause,
  parseDerive,
  parseClassDef,
  parseClassSups,
  junk,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero), void, when)
import Data.Char qualified as Char
import Data.Kind (Type)
import Data.Maybe (fromJust, isJust)
import Data.String (IsString (fromString))
import LambdaBuffers.Compiler.NamingCheck (pClassName, pConstrName, pFieldName, pModuleNamePart, pTyName)
import LambdaBuffers.Frontend.Syntax (ClassConstraint (ClassConstraint), ClassDef (ClassDef), ClassName (ClassName), ClassRef (ClassRef), ConstrName (ConstrName), Constraint (Constraint), Constructor (Constructor), Derive (Derive), Field (Field), FieldName (FieldName), Import (Import), InstanceClause (InstanceClause), Module (Module), ModuleAlias (ModuleAlias), ModuleName (ModuleName), ModuleNamePart (ModuleNamePart), Name (Name), Product (Product), Record (Record), SourceInfo (SourceInfo, to), SourcePos (SourcePos), Statement (StClassDef, StDerive, StInstanceClause, StTyDef), Sum (Sum), Ty (TyApp, TyRef', TyVar), TyArg (TyArg), TyBody (Opaque, ProductBody, RecordBody, SumBody), TyDef (TyDef), TyName (TyName), TyRef (TyRef), VarName (VarName), kwAs, kwClassDef, kwDerive, kwImport, kwInstance, kwModule, kwQualified, kwTyDefOpaque, kwTyDefProduct, kwTyDefRecord, kwTyDefSum, kws)
import Text.Parsec (ParseError, ParsecT, SourceName, Stream, alphaNum, between, char, endOfLine, eof, getPosition, label, lower, many, many1, manyTill, notFollowedBy, optionMaybe, runParserT, satisfy, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, string, try, unexpected, (<?>))

type Parser :: Type -> (Type -> Type) -> Type -> Type
type Parser s m a = ParsecT s () m a

-- Note: Syntactic Form of Lambda Buffer Files.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- See docs/syntax.md
--
-- Warning: In the production
--  classdef       -> 'class' [ constraintexps '<=' ] classname { varname }
-- this is not LL(1)! Either we live with what we currently have which has a
-- large 'try' around parsing @[ constraintexps '<=' ]@, or we move to an
-- LALR(1) parser generator which should has no issues with parsing this.
--
-- Note: Parser Implementation.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We use Parsec [1] to parse the grammar.
--
-- We have the following invariant.
--
-- - Whitespace Invariant: Each parser @pa@ assumes to starts at a
--   nonwhitespace character where whitespace is defined by the parser 'junk'
--   i.e., whitespace or comments.
--
-- Remark.
-- The Whitespace Invariant is originally from [2].
--
-- For the Whitespace Invariant to be initially true, @'runParser' pa@ calls
-- 'junk', then @pa@, then 'Text.Parsec.eof' which ensures:
--
--  1. the Whitespace Invariant is initially true for the parser @pa@; and
--
--  2. the entire input is consumed.
--
-- Then, to ensure that all "subparsers" maintain the Whitespace Invariant, we
-- introduce the parser combinator 'token' for which @'token' pa@ runs @pa@,
-- then runs 'junk' to ensure that any following parsers will start at a non
-- whitespace character.
--
-- Thus, if we want to parse the string @"pomeranian"@ we should write
--
-- > token (Text.Parser.string "pomeranian")
--
-- instead of
--
-- > -- do NOT do this since it will NOT maintain the Whitespace Invariant.
-- > Text.Parser.string "pomeranian"
--
-- References.
--
--  [1] Parsec: Direct Style Monadic Parser Combinators For the Real World by
--  Daan Leijen and Erik Meijer
--
--  [2] Monadic Parser Combinators by Graham Hutton and Erik Meijer

-- * Primitives

{- | @'token' pa@ runs the parser @pa@ with 'try' followed by 'junk' to remove
 whitespace. Moreover, this gets the 'SourceInfo' of the parsed token without
 the whitespace

 See [Note: Parser Implementation].
-}
token :: Stream s m Char => Parser s m a -> Parser s m (SourceInfo, a)
token pa = withSourceInfo (try $ fmap (\a srcInfo -> (srcInfo, a)) pa) <* junk

token' :: Stream s m Char => Parser s m a -> Parser s m a
token' = fmap snd . token

{- | 'junk' skips whitespace and comments.

 See [Note: Parser Implementation].
-}
junk :: forall s m. Stream s m Char => Parser s m ()
junk = void (many (spaces1 <|> comment))
  where
    spaces1 :: Parser s m ()
    spaces1 = void $ many1 (space <?> "")

    comment :: Parser s m ()
    comment =
      void $
        try (string "--" <?> "")
          *>
          -- Note: the 'try' for 'endOfLine' is necessary because of the
          -- overlapping instances of both parsers as we may note that
          -- 'endOfLine' parses \r\n and \n.
          manyTill (satisfy Char.isPrint) (try endOfLine)

{- | 'keyword' parses the provided keyword ensuring that the keyword does *not*
 overlap with varname tokens and fieldname tokens.
-}
keyword :: Stream s m Char => String -> Parser s m ()
keyword k = void $ string k *> notFollowedBy alphaNum

runParser :: Stream s IO Char => Parser s IO a -> SourceName -> s -> IO (Either ParseError a)
runParser p = runParserT (junk *> p <* eof) ()

-- * Lexical elements

--
-- - Functions which have @parse@ as a prefix simply parse the token
--
--  - Functions which have @token@ as a prefix wrap the corresponding @parse@
--  function with the 'token' function.

parseModuleNamePart :: Stream s m Char => Parser s m (ModuleNamePart SourceInfo)
parseModuleNamePart = withSourceInfo . label' "module part name" $ ModuleNamePart <$> pModuleNamePart

parseModuleName :: Stream s m Char => Parser s m (ModuleName SourceInfo)
parseModuleName = withSourceInfo . label' "module name" $ ModuleName <$> sepBy parseModuleNamePart (char '.')

tokenModuleName :: Stream s m Char => Parser s m (ModuleName SourceInfo)
tokenModuleName = token' parseModuleName

parseTyVarName :: Stream s m Char => Parser s m (VarName SourceInfo)
parseTyVarName = withSourceInfo . label' "type variable name" $ do
  v <- many1 lower
  notKeyword v
  return . VarName . fromString $ v

-- | 'notKeyword' tests if the string is not a keyword -- failing otherwise.
notKeyword :: Stream s m Char => String -> Parser s m ()
notKeyword v = when (v `elem` kws) $ unexpected "keyword"

-- | 'parseName' is a class or a type name
parseName :: Stream s m Char => Parser s m (Name SourceInfo)
parseName = withSourceInfo . label' "either class or type name" $ Name <$> pTyName

tokenName :: Stream s m Char => Parser s m (Name SourceInfo)
tokenName = token' parseName

parseTyName :: Stream s m Char => Parser s m (TyName SourceInfo)
parseTyName = withSourceInfo . label' "type name" $ TyName <$> pTyName

tokenTyName :: Stream s m Char => Parser s m (TyName SourceInfo)
tokenTyName = token' parseTyName

parseClassName :: Stream s m Char => Parser s m (ClassName SourceInfo)
parseClassName = withSourceInfo . label' "class name" $ ClassName <$> pClassName

tokenClassName :: Stream s m Char => Parser s m (ClassName SourceInfo)
tokenClassName = token' parseClassName

parseModuleAliasInRef :: Stream s m Char => Parser s m (ModuleAlias SourceInfo)
parseModuleAliasInRef =
  withSourceInfo . label' "module alias in type or class reference" $
    ModuleAlias <$> do
      -- some awkwardness with the 'try' here.
      -- Ideally, we should use the @.@ to be the first set to determine when
      -- to stop parsing this... but oh well...
      ps <- many1 (try (parseModuleNamePart <* char '.'))
      withSourceInfo . return $ ModuleName ps

parseModuleAliasInImport :: Stream s m Char => Parser s m (ModuleAlias SourceInfo)
parseModuleAliasInImport = withSourceInfo . label' "module alias in module import" $ ModuleAlias <$> parseModuleName

tokenModuleAliasInImport :: Stream s m Char => Parser s m (ModuleAlias SourceInfo)
tokenModuleAliasInImport = token' parseModuleAliasInImport

parseTyRef' :: Stream s m Char => Parser s m (TyRef SourceInfo)
parseTyRef' = withSourceInfo . label' "type reference" $ do
  mayAlias <- optionMaybe parseModuleAliasInRef
  TyRef mayAlias <$> parseTyName

parseTyVar :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTyVar = label' "type variable" $ TyVar <$> parseTyVarName

tokenTyVar :: Stream s m Char => Parser s m (Ty SourceInfo)
tokenTyVar = token' parseTyVar

parseTyRef :: Stream s m Char => Parser s m (Ty SourceInfo)
parseTyRef = withSourceInfo . label' "type reference" $ TyRef' <$> parseTyRef'

tokenTyRef :: Stream s m Char => Parser s m (Ty SourceInfo)
tokenTyRef = token' parseTyRef

parseFieldName :: Stream s m Char => Parser s m (FieldName SourceInfo)
parseFieldName =
  withSourceInfo . label' "record field name" $ do
    -- TODO(jaredponn): Technically, the specification says that field names
    -- are disjoint from keywords, but some of the other golden tests use this
    -- fact.
    -- We leave it in as a fairly harmless bug for now.
    --
    -- But the version that fixes this is as follows:
    --
    -- > v <- pFieldName
    -- > -- Recall in the lexical specification that fieldnames are disjoint from keywords
    -- > notKeyword $ Data.Text.unpack v
    -- > return $ FieldName v
    FieldName <$> pFieldName

tokenFieldName :: Stream s m Char => Parser s m (FieldName SourceInfo)
tokenFieldName = token' parseFieldName

parseConstructorName :: Stream s m Char => Parser s m (ConstrName SourceInfo)
parseConstructorName = withSourceInfo . label' "sum constructor name" $ ConstrName <$> pConstrName

tokenConstructorName :: Stream s m Char => Parser s m (ConstrName SourceInfo)
tokenConstructorName = token' parseConstructorName

parseTyArg :: Stream s m Char => Parser s m (TyArg SourceInfo)
parseTyArg = withSourceInfo . label' "type argument" $ do
  VarName vn _ <- parseTyVarName
  return $ TyArg vn

tokenTyArg :: Stream s m Char => Parser s m (TyArg SourceInfo)
tokenTyArg = token' parseTyArg

parseClassRef :: Stream s m Char => Parser s m (ClassRef SourceInfo)
parseClassRef = withSourceInfo . label' "class reference" $ do
  mayAlias <- optionMaybe parseModuleAliasInRef
  ClassRef mayAlias <$> parseClassName

tokenClassRef :: Stream s m Char => Parser s m (ClassRef SourceInfo)
tokenClassRef = token' parseClassRef

-- * Grammar

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
parseSexp = label' "s-expression" $ parseSexpAtom <|> parseSexpList

parseSexpAtom :: forall s m. Stream s m Char => Parser s m (Ty SourceInfo)
parseSexpAtom = tokenTyRef <|> tokenTyVar

parseTys :: forall s m. Stream s m Char => Parser s m [Ty SourceInfo]
parseTys = many parseSexp

parseSexpList :: forall s m. Stream s m Char => Parser s m (Ty SourceInfo)
parseSexpList = between (token (char '(')) (token (char ')')) (parseTys >>= tysToTy)

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
      (token (char '|'))
  return $ Sum cs

parseSumConstructor :: Stream s m Char => Parser s m (Constructor SourceInfo)
parseSumConstructor =
  withSourceInfo . label' "sum type constructor" $
    Constructor
      <$> tokenConstructorName
      <*> parseProduct

parseProduct :: Stream s m Char => Parser s m (Product SourceInfo)
parseProduct = withSourceInfo . label' "product type expression" $ Product <$> parseTys

parseRecord :: Stream s m Char => Parser s m (Record SourceInfo)
parseRecord = withSourceInfo . label' "record type expression" $ do
  fields <-
    between
      (token $ char '{')
      (token $ char '}')
      $ sepBy parseField (token $ char ',')
  return $ Record fields

parseField :: Stream s m Char => Parser s m (Field SourceInfo)
parseField = withSourceInfo . label' "record field" $ do
  fn <- tokenFieldName
  _ <- token $ char ':' *> notFollowedBy (char '-')
  -- Why is the @'notFollowedBy'@ here?
  -- Consider:
  -- > record A a = { fieldName :-- a }
  -- We want to parse the @:--@ as @:-@ and @-@ (the specification says this),
  -- but without the @'notFollowedBy'@, this would parse as @:@ and @--@ will
  -- start a comment.
  Field fn <$> parseTyTopLevel

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
  _ <- token $ keyword kw
  tyN <- tokenTyName
  args <- many tokenTyArg
  _ <- token $ char '='
  TyDef tyN args <$> parseBody

parseOpaqueTyDef :: Stream s m Char => Parser s m (TyDef SourceInfo)
parseOpaqueTyDef = withSourceInfo . label' "opaque type definition" $ do
  _ <- token (keyword kwTyDefOpaque)
  tyN <- tokenTyName
  args <- many tokenTyArg
  return $ TyDef tyN args Opaque

parseConstraint :: Stream s m Char => Parser s m (Constraint SourceInfo)
parseConstraint = withSourceInfo . label' "constraint" $ Constraint <$> tokenClassRef <*> parseTys

parseDerive :: Stream s m Char => Parser s m (Derive SourceInfo)
parseDerive = label' "derive statement" $ token (keyword kwDerive) >> Derive <$> parseConstraint

parseInstanceClause :: Stream s m Char => Parser s m (InstanceClause SourceInfo)
parseInstanceClause = withSourceInfo . label' "instance clause" $ do
  _ <- token (keyword kwInstance)
  clauseHead <- parseConstraint
  mayBodyFollows <- optionMaybe (token (string ":-"))
  case mayBodyFollows of
    Nothing -> return $ InstanceClause clauseHead []
    Just _ -> InstanceClause clauseHead <$> parseInstanceBody

parseInstanceBody :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseInstanceBody = parseConstraints

-- | Constraints sexp.
parseConstraintSexp :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraintSexp = parseConstraintList <|> parseConstraintAtom

parseConstraintAtom :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraintAtom = pure <$> parseConstraint

parseConstraintList :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraintList = between (token (char '(')) (token (char ')')) parseConstraints

parseConstraints :: Stream s m Char => Parser s m [Constraint SourceInfo]
parseConstraints = concat <$> sepBy parseConstraintSexp (token (char ','))

parseClassDef :: Stream s m Char => Parser s m (ClassDef SourceInfo)
parseClassDef = withSourceInfo . label' "class definition" $ do
  _ <- token (keyword kwClassDef)
  maySups <-
    optionMaybe
      -- Remark: parsing this is problematic for LL(1) parsers, hence the
      -- rather large 'try'.
      -- We really are abusing the infinite look ahead here...
      ( try $ do
          sups <- parseClassSups
          _ <- token (string "<=")
          return sups
      )
  clName <- tokenClassName
  clArgs <- parseClassArgs
  case maySups of
    Nothing -> return $ ClassDef clName clArgs []
    Just sups -> return $ ClassDef clName clArgs sups

parseClassArgs :: Stream s m Char => Parser s m [TyArg SourceInfo]
parseClassArgs = label' "class args" $ many tokenTyArg

-- | ClassCnstrs sexp.
parseClassCnstrSexp :: Stream s m Char => Parser s m [ClassConstraint SourceInfo]
parseClassCnstrSexp = parseClassCnstrList <|> parseClassCnstrAtom

parseClassCnstrAtom :: Stream s m Char => Parser s m [ClassConstraint SourceInfo]
parseClassCnstrAtom = pure <$> parseClassCnstr

parseClassCnstrList :: Stream s m Char => Parser s m [ClassConstraint SourceInfo]
parseClassCnstrList = between (token $ char '(') (token $ char ')') parseClassSups

-- FIXME(bladyjoker): Should accept "Eq a "
parseClassSups :: Stream s m Char => Parser s m [ClassConstraint SourceInfo]
parseClassSups = concat <$> sepBy parseClassCnstrSexp (token (char ','))

parseClassCnstr :: Stream s m Char => Parser s m (ClassConstraint SourceInfo)
parseClassCnstr =
  label' "class constraint" $
    ClassConstraint <$> tokenClassRef <*> parseClassArgs

parseStatement :: Stream s m Char => Parser s m (Statement SourceInfo)
parseStatement =
  (StTyDef <$> parseTyDef)
    <|> (StClassDef <$> parseClassDef)
    <|> (StInstanceClause <$> parseInstanceClause)
    <|> (StDerive <$> parseDerive)

parseModule :: Stream s m Char => Parser s m (Module SourceInfo)
parseModule = withSourceInfo . label' "module definition" $ do
  _ <- token $ keyword kwModule
  modName <- tokenModuleName
  imports <- many parseImport
  stmnts <- many parseStatement
  return $ Module modName imports stmnts

parseImport :: Stream s m Char => Parser s m (Import SourceInfo)
parseImport = label' "import statement" $ do
  -- Getting the starting position
  (srcInfo, _) <- token $ keyword kwImport

  isQual <- isJust <$> optionMaybe (token $ keyword kwQualified)
  modName@(ModuleName _ nameSrcInfo) <- tokenModuleName

  mayModAlias <- optionMaybe $ token (keyword kwAs) *> tokenModuleAliasInImport

  mayBracketSrcInfoAndNames <- optionMaybe $ do
    _ <- token $ char '('
    names <- sepEndBy tokenName (token $ char ',')
    (bracketSrcInfo, _) <- token $ char ')'
    return (bracketSrcInfo, names)

  let mayBracketSrcInfo = fmap fst mayBracketSrcInfoAndNames
      mayNames = fmap snd mayBracketSrcInfoAndNames

  return $
    Import isQual modName mayNames mayModAlias $
      srcInfo
        { to =
            -- Get the rightmost position of the rightmost parsed token
            -- Note: the 'fromJust' clearly never fails.
            fromJust $
              fmap to mayBracketSrcInfo
                <|> ( case mayModAlias of
                        Just (ModuleAlias _ modAliasSrcInfo) -> Just $ to modAliasSrcInfo
                        _ -> Nothing
                    )
                <|> fmap to (Just nameSrcInfo)
        }

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
