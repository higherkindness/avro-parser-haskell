{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Avro.Parser where

import Data.Avro
import Data.Avro.Schema as S
import qualified Data.Avro.Types as AT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

spaceConsumer :: MonadParsec Char T.Text m => m ()
spaceConsumer =
  L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol = L.symbol spaceConsumer

reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved = lexeme . chunk

number :: (MonadParsec Char T.Text m, Integral a) => m a
number = L.signed spaceConsumer (lexeme L.decimal) <|> lexeme L.octal <|> lexeme L.hexadecimal

floating :: (MonadParsec Char T.Text m, RealFloat a) => m a
floating = L.signed spaceConsumer (lexeme L.float)

stringLiteral :: MonadParsec Char T.Text m => m T.Text
stringLiteral = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

betweenBraces :: MonadParsec Char T.Text m => m a -> m a
betweenBraces = between (symbol "{") (symbol "}")

betweenSquares :: MonadParsec Char T.Text m => m a -> m a
betweenSquares = between (symbol "[") (symbol "]")

betweenParens :: MonadParsec Char T.Text m => m a -> m a
betweenParens = between (symbol "(") (symbol ")")

betweenDiamonds :: MonadParsec Char T.Text m => m a -> m a
betweenDiamonds = between (symbol "<") (symbol ">")

ident :: MonadParsec Char T.Text m => m T.Text
ident =
  (\h t -> T.pack (h : t)) <$> letterChar <*> many (alphaNumChar <|> char '_')

identifier :: MonadParsec Char T.Text m => m T.Text
identifier = lexeme ident

parseFullName :: T.Text -> TypeName
parseFullName (T.splitOn "." -> components) = S.TN {baseName, namespace}
  where
    baseName = last components
    namespace = filter (/= "") $ init components

schemaType :: MonadParsec Char T.Text m => m S.Type
schemaType =
  Null <$ reserved "null"
    <|> Boolean <$ reserved "boolean"
    <|> Int <$ reserved "int"
    <|> Long <$ reserved "long"
    <|> Float <$ reserved "float"
    <|> Double <$ reserved "double"
    <|> Bytes <$ reserved "bytes"
    <|> String <$ reserved "string"
    <|> Array <$ reserved "array" <*> betweenDiamonds schemaType
    <|> Map <$ reserved "map" <*> betweenDiamonds schemaType
-- TODO:
-- <|> NamedType <$> (symbol "@" *> betweenParens S.parseFullname)
-- <|> Record <$ reserved "record"
-- <|> Enum <$ reserved "enum"
-- <|> Union <$ reserved "union"
-- <|> Fixed <$ reserved "fixed"
