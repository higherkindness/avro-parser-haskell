{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Avro.Parser where

import Data.Avro
import Data.Avro.Schema
import qualified Data.Avro.Types as AT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)
import Language.Avro.Types
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
  (T.pack .) . (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '-')

identifier :: MonadParsec Char T.Text m => m T.Text
identifier = lexeme ident

parseNamedType :: [T.Text] -> TypeName
parseNamedType [] = error "named types cannot be empty"
parseNamedType xs = TN {baseName, namespace}
  where
    baseName = last xs
    namespace = filter (/= "") $ init xs

parseAnnotation :: MonadParsec Char T.Text m => m Annotation
parseAnnotation =
  symbol "@"
    *> ( Namespace <$ reserved "namespace" <*> betweenParens stringLiteral
           <|> Order <$ reserved "order" <*> betweenParens stringLiteral
           <|> Aliases <$ reserved "aliases"
             <*> betweenParens
               (betweenSquares $ lexeme $ sepBy1 stringLiteral $ symbol ",")
           <|> OtherAnnotation <$> identifier <*> betweenParens stringLiteral
       )

parseImport :: MonadParsec Char T.Text m => m ImportType
parseImport =
  reserved "import"
    *> ( impHelper IdlImport "idl"
           <|> impHelper ProtocolImport "protocol"
           <|> impHelper SchemaImport "schema"
       )
  where
    impHelper :: MonadParsec Char T.Text m => (T.Text -> a) -> T.Text -> m a
    impHelper ct t = ct <$> (reserved t *> stringLiteral <* symbol ";")

parseProtocol :: MonadParsec Char T.Text m => m Protocol
parseProtocol =
  Protocol <$ reserved "protocol" <*> identifier
    <*> betweenBraces (many parseImport) -- TODO: here goes more things!

parseVector :: MonadParsec Char T.Text m => m a -> m (Vector a)
parseVector t = fromList <$> betweenBraces (lexeme $ sepBy1 t $ symbol ",")

schemaType :: MonadParsec Char T.Text m => m Schema
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
    <|> Union <$ reserved "union" <*> parseVector schemaType -- TODO: parse Aliases here! 👇🏼
    <|> Fixed <$ reserved "fixed" <*> (parseNamedType . pure <$> identifier) <*> pure [] <*> betweenParens number <* symbol ";"
    <|> Enum <$ reserved "enum" <*> (parseNamedType . pure <$> identifier) <*> pure [] <*> pure Nothing <*> parseVector identifier
    -- TODO: <|> Record <$ reserved "record"
    <|> NamedType . parseNamedType <$> lexeme (sepBy1 identifier $ char '.')
