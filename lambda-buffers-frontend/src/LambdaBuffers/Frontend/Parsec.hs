module LambdaBuffers.Frontend.Parsec where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((&), (.~))
import Control.Monad (MonadPlus (mzero))
import Data.Kind (Type)
import Data.ProtoLens (Message (defMessage))
import Data.ProtoLens.Field (HasField)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Proto.Compiler (SourceInfo, SourcePosition, Ty)
import Proto.Compiler_Fields (column, file, localTyRef, name, posFrom, posTo, row, sourceInfo, tyApp, tyArgs, tyFunc, tyName, tyRef, tyVar, varName)
import Text.Parsec (ParsecT, Stream, alphaNum, char, eof, getPosition, getState, label, lower, many, many1, parserTrace, runParserT, sepBy, sepEndBy, sourceColumn, sourceLine, sourceName, space, try, (<?>))
import Text.Parsec.Char (upper)

-- parseModule :: Stream s m Char => ParsecT s u m Module
-- parseModule = do
--   void $ P.symbol lambdaBuffersLexer "module"
--   modName <- P.identifier lambdaBuffersLexer
--   return $ defMessage & moduleName . name .~ fromString modName

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

parseModuleNamePart :: Stream s m Char => Parser s m Text
parseModuleNamePart = fromString <$> ((:) <$> upper <*> many alphaNum) <?> "module name part"

parseModuleName :: Stream s m Char => Parser s m [Text]
parseModuleName = sepBy parseModuleNamePart (char '.') <?> "module name"

parseTyVarName :: Stream s m Char => Parser s m Text
parseTyVarName = fromString <$> many1 lower <?> "type variable name"

parseTyRefName :: Stream s m Char => Parser s m Text
parseTyRefName = fromString <$> ((:) <$> upper <*> many alphaNum) <?> "type reference name"

label' :: String -> Parser s m a -> Parser s m a
label' l m = label m l

parseTyVar :: Stream s m Char => Parser s m Ty
parseTyVar = withSourceInfo . label' "type variable" $ do
  varN <- parseTyVarName
  parserTrace $ "tyVar " <> show varN
  return $ defMessage & tyVar . varName . name .~ varN

-- TODO: Handle ForeignRefs
parseTyRef :: Stream s m Char => Parser s m Ty
parseTyRef = withSourceInfo . label' "type reference" $ do
  refN <- parseTyRefName
  parserTrace $ "tyRef " <> show refN
  return $ defMessage & tyRef . localTyRef . tyName . name .~ refN

parseTy :: Stream s m Char => Parser s m Ty
parseTy = (try parseTys >>= tysToTy) <?> "top level type expression"

parseTy' :: Stream s m Char => Parser s m Ty
parseTy' =
  label' "non-top leve type expression" $
    parseTyRef
      <|> parseTyVar
      <|> ( (char '(' >> many space)
              *> (try parseTys >>= tysToTy)
              <* (many space >> char ')')
          )

parseTys :: Stream s m Char => Parser s m [Ty]
parseTys = sepEndBy parseTy' (many1 space) <?> "type list"

tysToTy :: [Ty] -> Parser s m Ty
tysToTy tys = case tys of
  [] -> mzero
  [f] -> return f
  f : as ->
    return $
      defMessage
        & tyApp . tyFunc .~ f
        & tyApp . tyArgs .~ as

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
