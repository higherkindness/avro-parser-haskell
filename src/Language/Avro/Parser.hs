{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Avro.Parser where

import Data.Avro
import Data.Avro.Schema
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList)
import Language.Avro.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

spaces :: MonadParsec Char T.Text m => m ()
spaces = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme = L.lexeme spaces

symbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol = L.symbol spaces

reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved = lexeme . chunk

number :: (MonadParsec Char T.Text m, Integral a) => m a
number = L.signed spaces (lexeme L.decimal) <|> lexeme L.octal <|> lexeme L.hexadecimal

floating :: (MonadParsec Char T.Text m, RealFloat a) => m a
floating = L.signed spaces (lexeme L.float)

strlit :: MonadParsec Char T.Text m => m T.Text
strlit = T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

braces :: MonadParsec Char T.Text m => m a -> m a
braces = between (symbol "{") (symbol "}")

brackets :: MonadParsec Char T.Text m => m a -> m a
brackets = between (symbol "[") (symbol "]")

parens :: MonadParsec Char T.Text m => m a -> m a
parens = between (symbol "(") (symbol ")")

diamonds :: MonadParsec Char T.Text m => m a -> m a
diamonds = between (symbol "<") (symbol ">")

backticks :: MonadParsec Char T.Text m => m T.Text
backticks = T.pack <$> (char '`' >> manyTill L.charLiteral (char '`'))

ident :: MonadParsec Char T.Text m => m T.Text
ident = T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '-'))

identifier :: MonadParsec Char T.Text m => m T.Text
identifier = lexeme (ident <|> backticks)

toNamedType :: [T.Text] -> TypeName
toNamedType [] = error "named types cannot be empty"
toNamedType xs = TN {baseName, namespace}
  where
    baseName = last xs
    namespace = filter (/= "") $ init xs

multiNamedTypes :: [T.Text] -> [TypeName]
multiNamedTypes = fmap $ toNamedType . T.splitOn "."

parseAnnotation :: MonadParsec Char T.Text m => m Annotation
parseAnnotation = Annotation <$ symbol "@" <*> identifier <*> parens strlit

parseNamespace :: MonadParsec Char T.Text m => m Namespace
parseNamespace = toNs <$ (symbol "@" *> reserved "namespace") <*> parens strlit
  where
    toNs :: T.Text -> Namespace
    toNs = Namespace . T.splitOn "."

parseAliases :: MonadParsec Char T.Text m => m Aliases
parseAliases = multiNamedTypes <$> parseFieldAlias

parseImport :: MonadParsec Char T.Text m => m ImportType
parseImport =
  reserved "import"
    *> ( impHelper IdlImport "idl"
           <|> impHelper ProtocolImport "protocol"
           <|> impHelper SchemaImport "schema"
       )
  where
    impHelper :: MonadParsec Char T.Text m => (T.Text -> a) -> T.Text -> m a
    impHelper ct t = ct <$> (reserved t *> strlit <* symbol ";")

parseProtocol :: MonadParsec Char T.Text m => m Protocol
parseProtocol =
  buildProtocol <$> optional parseNamespace <* reserved "protocol"
    <*> identifier
    <*> braces (many serviceThing)
  where
    buildProtocol :: Maybe Namespace -> T.Text -> [ProtocolThing] -> Protocol
    buildProtocol ns name things =
      Protocol
        ns
        name
        [i | ProtocolThingImport i <- things]
        [t | ProtocolThingType t <- things]
        [m | ProtocolThingMethod m <- things]

data ProtocolThing
  = ProtocolThingImport ImportType
  | ProtocolThingType Schema
  | ProtocolThingMethod Method

serviceThing :: MonadParsec Char T.Text m => m ProtocolThing
serviceThing =
  ProtocolThingImport <$> parseImport
  <|> ProtocolThingType <$> parseSchema
  <|> ProtocolThingMethod <$> parseMethod

parseVector :: MonadParsec Char T.Text m => m a -> m (Vector a)
parseVector t = fromList <$> braces (lexeme $ sepBy1 t $ symbol ",")

parseTypeName :: MonadParsec Char T.Text m => m TypeName
parseTypeName = toNamedType . pure <$> identifier

parseOrder :: MonadParsec Char T.Text m => m Order
parseOrder =
  symbol "@" *> reserved "order"
    *> parens
      ( Ascending <$ string "\"ascending\""
          <|> Descending <$ string "\"descending\""
          <|> Ignore <$ string "\"ignore\""
      )

parseFieldAlias :: MonadParsec Char T.Text m => m [T.Text]
parseFieldAlias =
  symbol "@" *> reserved "aliases"
    *> parens (brackets $ lexeme $ sepBy1 strlit $ symbol ",")

parseField :: MonadParsec Char T.Text m => m Field
parseField =
  (\o t a n -> Field n a Nothing o t Nothing) -- ignore docs and default values for now...
    <$> optional parseOrder
    <*> parseSchema
    <*> option [] parseFieldAlias
    <*> identifier
    <* symbol ";"

parseArgument :: MonadParsec Char T.Text m => m Argument
parseArgument = Argument <$> parseSchema <*> identifier

parseMethod :: MonadParsec Char T.Text m => m Method
parseMethod =
  (\r n a t o -> Method n a r t o)
    <$> parseSchema
    <*> identifier
    <*> parens (option [] (lexeme $ sepBy1 parseArgument $ symbol ","))
    <*> option Null (reserved "throws" *> parseSchema)
    <*> option False (True <$ reserved "oneway")
    <* symbol ";"

parseSchema :: MonadParsec Char T.Text m => m Schema
parseSchema =
  Null <$ (reserved "null" <|> reserved "void")
    <|> Boolean <$ reserved "boolean"
    <|> Int <$ reserved "int"
    <|> Long <$ reserved "long"
    <|> Float <$ reserved "float"
    <|> Double <$ reserved "double"
    <|> Bytes <$ reserved "bytes"
    <|> String <$ reserved "string"
    <|> Array <$ reserved "array" <*> diamonds parseSchema
    <|> Map <$ reserved "map" <*> diamonds parseSchema
    <|> Union <$ reserved "union" <*> parseVector parseSchema
    <|> try
      ( flip Fixed
          <$> option [] parseAliases <* reserved "fixed"
          <*> parseTypeName
          <*> parens number
      )
    <|> try
      ( flip Enum
          <$> option [] parseAliases <* reserved "enum"
          <*> parseTypeName
          <*> pure Nothing -- docs are ignored for now...
          <*> parseVector identifier
      )
    <|> try
      ( flip Record
          <$> option [] parseAliases <* reserved "record"
          <*> parseTypeName
          <*> pure Nothing -- docs are ignored for now...
          <*> optional parseOrder -- TODO: the order of a record is not here!
          <*> option [] (braces . many $ parseField)
      )
    <|> NamedType . toNamedType <$> lexeme (sepBy1 identifier $ char '.')
