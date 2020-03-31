{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parser for AVRO (@.avdl@) files, as defined in <http://avro.apache.org/docs/1.8.2/spec.html>.
module Language.Avro.Parser
  ( -- * Main parsers
    parseProtocol,
    readWithImports,

    -- * Intermediate parsers
    parseAliases,
    parseAnnotation,
    parseDecimal,
    parseImport,
    parseMethod,
    parseNamespace,
    parseOrder,
    parseSchema,
  )
where

import Control.Monad (filterM)
import Data.Avro
import Data.Either (partitionEithers)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Language.Avro.Types
import System.Directory (doesFileExist)
import System.FilePath
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

-- | Parses annotations into the 'Annotation' structure.
parseAnnotation :: MonadParsec Char T.Text m => m Annotation
parseAnnotation = Annotation <$ symbol "@" <*> identifier <*> parens strlit

-- | Parses a single import into the 'ImportType' structure.
parseNamespace :: MonadParsec Char T.Text m => m Namespace
parseNamespace = toNs <$ (symbol "@" *> reserved "namespace") <*> parens strlit
  where
    toNs :: T.Text -> Namespace
    toNs = Namespace . T.splitOn "."

-- | Parses aliases, which are just Lists of 'TypeName'.
parseAliases :: MonadParsec Char T.Text m => m Aliases
parseAliases = multiNamedTypes <$> parseFieldAlias

-- | Parses a single import into the 'ImportType' structure.
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

-- | Parses a single protocol into the 'Protocol' structure.
parseProtocol :: MonadParsec Char T.Text m => m Protocol
parseProtocol =
  buildProtocol <$ spaces <*> optional parseNamespace <* reserved "protocol"
    <*> identifier
    <*> braces (many serviceThing)
  where
    buildProtocol :: Maybe Namespace -> T.Text -> [ProtocolThing] -> Protocol
    buildProtocol ns name things =
      Protocol
        ns
        name
        (S.fromList [i | ProtocolThingImport i <- things])
        (S.fromList [t | ProtocolThingType t <- things])
        (S.fromList [m | ProtocolThingMethod m <- things])

data ProtocolThing
  = ProtocolThingImport ImportType
  | ProtocolThingType Schema
  | ProtocolThingMethod Method

serviceThing :: MonadParsec Char T.Text m => m ProtocolThing
serviceThing =
  try (ProtocolThingImport <$> parseImport)
    <|> try (ProtocolThingMethod <$> parseMethod)
    <|> ProtocolThingType <$> parseSchema <* optional (symbol ";")

parseVector :: MonadParsec Char T.Text m => m a -> m (V.Vector a)
parseVector t = V.fromList <$> braces (lexeme $ sepBy1 t $ symbol ",")

parseTypeName :: MonadParsec Char T.Text m => m TypeName
parseTypeName = toNamedType . pure <$> identifier

-- | Parses order annotations into the 'Order' structure.
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
  (\o t a n -> Field n a Nothing o t Nothing) -- FIXME: docs are not supported yet.
    <$> optional parseOrder
    <*> parseSchema
    <*> option [] parseFieldAlias
    <*> identifier
    <* (symbol ";" <|> symbol "=" <* manyTill anySingle (symbol ";")) -- FIXME: default values are not supported yet.

-- | Parses arguments of methods into the 'Argument' structure.
parseArgument :: MonadParsec Char T.Text m => m Argument
parseArgument = Argument <$> parseSchema <*> identifier

-- | Parses a single method/message into the 'Method' structure.
parseMethod :: MonadParsec Char T.Text m => m Method
parseMethod =
  (\r n a t o -> Method n a r t o)
    <$> parseSchema
    <*> identifier
    <*> parens (option [] (lexeme $ sepBy1 parseArgument $ symbol ","))
    <*> option Null (reserved "throws" *> parseSchema)
    <*> option False (True <$ reserved "oneway")
    <* symbol ";"

-- | Parses the special type @decimal@ into it's corresponding 'Decimal' structure.
parseDecimal :: MonadParsec Char T.Text m => m Decimal
parseDecimal = toDec <$ reserved "decimal" <*> parens (lexeme $ sepBy1 number $ symbol ",")
  where
    toDec :: [Int] -> Decimal
    toDec [precision] = Decimal (fromIntegral precision) 0
    toDec [precision, scale] = Decimal (fromIntegral precision) (fromIntegral scale)
    toDec _ = error "decimal types can only be specified using two numbers!"

-- | Parses a single type respecting @Data.Avro.Schema@'s 'Schema'.
parseSchema :: MonadParsec Char T.Text m => m Schema
parseSchema =
  Null <$ (reserved "null" <|> reserved "void")
    <|> Boolean <$ reserved "boolean"
    <|> Int' <$ reserved "int"
    <|> Int (Just Date) <$ reserved "date"
    <|> Int (Just TimeMillis) <$ reserved "time_ms"
    <|> Long' <$ reserved "long"
    <|> Long . Just . DecimalL <$> parseDecimal
    <|> Long (Just TimestampMillis) <$ reserved "timestamp_ms"
    <|> Float <$ reserved "float"
    <|> Double <$ reserved "double"
    <|> Bytes' <$ reserved "bytes"
    <|> String (Just UUID) <$ reserved "uuid"
    <|> String' <$ reserved "string"
    <|> Array <$ reserved "array" <*> diamonds parseSchema
    <|> Map <$ reserved "map" <*> diamonds parseSchema
    <|> Union <$ reserved "union" <*> parseVector parseSchema
    <|> try
      ( flip Fixed
          <$> option [] parseAliases <* reserved "fixed"
          <*> parseTypeName
          <*> parens number
          <*> pure Nothing
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
          <$> option [] parseAliases <* (reserved "record" <|> reserved "error")
          <*> parseTypeName
          <*> pure Nothing -- docs are ignored for now...
#if MIN_VERSION_avro(0,5,1)
#else
          <*> optional parseOrder -- FIXME: order for records is not supported yet.
#endif
          <*> option [] (braces . many $ parseField)
      )
    <|> NamedType . toNamedType <$> lexeme (sepBy1 identifier $ char '.')

parseFile :: Parsec e T.Text a -> String -> IO (Either (ParseErrorBundle T.Text e) a)
parseFile p file = runParser p file <$> T.readFile file

(>>>=) :: Applicative m => Either a b -> (b -> m (Either a c)) -> m (Either a c)
Left x >>>= _ = pure (Left x)
Right y >>>= f = f y

-- | Reads and parses a whole file and its imports, recursively.
readWithImports ::
  -- | base directory
  FilePath ->
  -- | initial file
  FilePath ->
  IO (Either (ParseErrorBundle T.Text Char) Protocol)
readWithImports baseDir initialFile = do
  initial <- parseFile parseProtocol (baseDir </> initialFile)
  case initial of
    Left e -> pure $ Left e
    Right p -> do
      possibleImps <- traverse (oneOfTwo . T.unpack) [i | IdlImport i <- S.toList $ imports p]
      (lefts, rights) <- partitionEithers <$> traverse (>>>= readWithImports baseDir) possibleImps
      pure $ case lefts of
        e : _ -> Left e
        _ -> Right $ S.foldl' (<>) p (S.fromList rights)
  where
    oneOfTwo :: FilePath -> IO (Either (ParseErrorBundle T.Text Char) FilePath)
    oneOfTwo p = do
      let dir = takeDirectory initialFile
          path1 = baseDir </> p
          path2 = baseDir </> dir </> p
      options <- (,) <$> doesFileExist path1 <*> doesFileExist path2
      pure $ case options of
        (True, False) -> Right p
        (False, True) -> Right $ dir </> p
        (False, False) -> runParser (fail $ "Import not found: " ++ p) initialFile ""
        (True, True)
          | normalise dir == "." -> Right p
          | otherwise -> runParser (fail $ "Duplicate files found: " ++ p) initialFile ""
