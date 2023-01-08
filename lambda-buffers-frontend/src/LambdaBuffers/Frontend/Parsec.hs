module LambdaBuffers.Frontend.Parsec where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((&), (.~))
import Control.Monad (MonadPlus (mzero), void)
import Data.Kind (Type)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Field (HasField)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Proto.Compiler (ConstrName, Kind'KindRef (Kind'KIND_REF_TYPE), Module, ModuleName, ModuleNamePart, Product, SourceInfo, SourcePosition, Sum'Constructor, Ty, TyArg, TyBody, TyDef, TyName, VarName)
import Proto.Compiler_Fields (argKind, argName, column, constrName, constructors, fields, file, kindRef, localTyRef, moduleName, name, ntuple, opaque, parts, posFrom, posTo, row, sourceInfo, tyApp, tyArgs, tyBody, tyFunc, tyName, tyRef, tyVar, typeDefs, varName)
import Proto.Compiler_Fields qualified as P
import Text.Parsec (ParsecT, Stream, alphaNum, char, endOfLine, eof, getPosition, getState, label, lower, many, many1, optionMaybe, optional, runParserT, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, string, try, (<?>))
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
parseModuleNamePart = withSourceInfo . label' "module part name" $ do
  mpn <- parseUpperCamelCase
  return $ defMessage & name .~ mpn

parseModuleName :: Stream s m Char => Parser s m ModuleName
parseModuleName = withSourceInfo . label' "module name" $ do
  mpns <- sepBy parseModuleNamePart (char '.')
  return $ defMessage & parts .~ mpns

parseTyVarName :: Stream s m Char => Parser s m VarName
parseTyVarName = withSourceInfo . label' "type variable name" $ do
  vn <- fromString <$> many1 lower
  return $ defMessage & name .~ vn

parseTyRefName :: Stream s m Char => Parser s m TyName
parseTyRefName = withSourceInfo . label' "type reference name" $ do
  rn <- parseUpperCamelCase
  return $ defMessage & name .~ rn

parseTyVar :: Stream s m Char => Parser s m Ty
parseTyVar = do
  tyV <- withSourceInfo . label' "type variable" $ do
    varN <- parseTyVarName
    return $ defMessage & varName .~ varN
  return (defMessage & tyVar .~ tyV)

-- TODO: Handle ForeignRefs
parseTyRef :: Stream s m Char => Parser s m Ty
parseTyRef = do
  tyR <- withSourceInfo . label' "type reference" $ do
    refN <- parseTyRefName
    return $ defMessage & localTyRef . tyName .~ refN
  return $ defMessage & tyRef .~ tyR

parseTy :: Stream s m Char => Parser s m Ty
parseTy = withSourceInfo . label' "top level type expression" $ try parseTys >>= tysToTy

parseTys :: Stream s m Char => Parser s m [Ty]
parseTys = sepEndBy parseTy' (many1 lineSpace) <?> "type list"

parseTy' :: Stream s m Char => Parser s m Ty
parseTy' =
  withSourceInfo . label' "non-top level type expression" $
    parseTyRef
      <|> parseTyVar
      <|> ( (char '(' >> many lineSpace)
              *> (try parseTys >>= tysToTy)
              <* (many lineSpace >> char ')')
          )

tysToTy :: [Ty] -> Parser s m Ty
tysToTy tys = case tys of
  [] -> mzero
  [ty] -> return ty
  f : as ->
    return $
      defMessage
        & tyApp . tyFunc .~ f
        & tyApp . tyArgs .~ as

parseSumBody :: Stream s m Char => Parser s m TyBody
parseSumBody = do
  s <- withSourceInfo . label' "sum type body" $ do
    cs <-
      sepBy
        parseSumConstructor
        (char '|' >> many1 lineSpace)
    return $ defMessage & constructors .~ cs
  return $ defMessage & P.sum .~ s

parseSumConstructor :: Stream s m Char => Parser s m Sum'Constructor
parseSumConstructor = label' "sum type constructor" $ do
  cn <- parseConstructorName
  p <- parseProduct
  return $
    defMessage
      & constrName .~ cn
      & P.product .~ p

parseProduct :: Stream s m Char => Parser s m Product
parseProduct = do
  maySpace <- optionMaybe lineSpace
  case maySpace of
    Nothing -> withSourceInfo . label' "empty constructor" $ do
      return $ defMessage & ntuple .~ defMessage
    Just _ -> do
      nt <- withSourceInfo . label' "type product" $ do
        _ <- many lineSpace
        tys <- parseTys
        return $ defMessage & fields .~ tys
      return $ defMessage & ntuple .~ nt

parseConstructorName :: Stream s m Char => Parser s m ConstrName
parseConstructorName = withSourceInfo . label' "sum constructor name" $ do
  rn <- parseUpperCamelCase
  return $ defMessage & name .~ rn

parseTyDef :: Stream s m Char => Parser s m TyDef
parseTyDef = label' "type definition" $ parseSumTyDef <|> parseOpaqueTyDef

parseSumTyDef :: Stream s m Char => Parser s m TyDef
parseSumTyDef = withSourceInfo . label' "sum type definition" $ do
  _ <- string "sum"
  _ <- many1 lineSpace
  tyN <- parseTyRefName
  _ <- many1 lineSpace
  args <- sepEndBy parseTyArg (many1 lineSpace)
  _ <- char '='
  _ <- many1 lineSpace
  body <- parseSumBody
  return $
    defMessage
      & tyName .~ tyN
      & tyArgs .~ args
      & tyBody .~ body

parseOpaqueTyDef :: Stream s m Char => Parser s m TyDef
parseOpaqueTyDef = withSourceInfo . label' "opaque type definition" $ do
  _ <- string "opaque"
  _ <- many1 lineSpace
  tyN <- parseTyRefName
  maySpace <- optionMaybe lineSpace
  args <- case maySpace of
    Nothing -> many lineSpace >> return []
    Just _ -> do
      _ <- many lineSpace
      sepBy parseTyArg (many1 lineSpace)
  return $
    defMessage
      & tyName .~ tyN
      & tyArgs .~ args
      & tyBody . opaque .~ defMessage

parseTyArg :: Stream s m Char => Parser s m TyArg
parseTyArg = withSourceInfo . label' "type argument" $ do
  vn <- parseTyVarName
  return $
    defMessage
      & argName .~ vn
      & argKind . kindRef .~ Kind'KIND_REF_TYPE

lineSpace :: Stream s m Char => Parser s m ()
lineSpace = label' "line space" $ void $ try $ do
  optional endOfLine
  char ' ' <|> char '\t'

lbNewLine :: Stream s m Char => Parser s m ()
lbNewLine = label' "lb new line" $ void endOfLine

parseModule :: Stream s m Char => Parser s m Module
parseModule = do
  _ <- string "module"
  _ <- many1 lineSpace
  modName <- parseModuleName
  _ <- many lineSpace
  _ <- many1 lbNewLine
  tyDs <- sepBy parseTyDef (many1 lbNewLine)
  _ <- many space
  return $
    defMessage
      & moduleName .~ modName
      & typeDefs .~ tyDs

getSourcePosition :: Stream s m Char => Parser s m SourcePosition
getSourcePosition = do
  pos <- getPosition
  return $
    defMessage
      & column .~ (fromIntegral . sourceColumn $ pos)
      & row .~ (fromIntegral . sourceLine $ pos)

withSourceInfo :: HasField a "sourceInfo" SourceInfo => Stream s m Char => Parser s m a -> Parser s m a
withSourceInfo p = do
  PS debug <- getState
  if not debug
    then p
    else do
      pos <- getSourcePosition
      x <- p
      pos' <- getSourcePosition
      filename <- fromString . sourceName <$> getPosition
      let xWithSourceInfo =
            x
              & sourceInfo
                .~ ( defMessage
                      & file .~ filename
                      & posFrom .~ pos
                      & posTo .~ pos'
                   )
      return xWithSourceInfo

label' :: String -> Parser s m a -> Parser s m a
label' l m = label m l
