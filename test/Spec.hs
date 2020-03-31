{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Avro
import qualified Data.Text as T
import Data.Vector (fromList)
import Language.Avro.Parser
import Language.Avro.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)
import Text.Megaparsec.Error

instance ShowErrorComponent Char where
  showErrorComponent = show

enumTest :: T.Text
enumTest =
  T.unlines
    [ "@aliases([\"org.foo.KindOf\"])",
      "enum Kind {",
      "FOO,",
      "BAR, // the bar enum value",
      "BAZ",
      "}"
    ]

simpleProtocol :: [T.Text]
simpleProtocol =
  [ "@namespace(\"example.seed.server.protocol.avro\")",
    "protocol PeopleService {",
    "import idl \"People.avdl\";",
    "example.seed.server.protocol.avro.PeopleResponse getPerson(example.seed.server.protocol.avro.PeopleRequest request);",
    "}"
  ]

simpleRecord :: T.Text
simpleRecord =
  T.unlines
    [ "@aliases([\"org.foo.Person\"])",
      "record Person {",
      "string name;",
      "int age;",
      "date birthday;",
      "}"
    ]

complexRecord :: T.Text
complexRecord =
  T.unlines
    [ "record TestRecord {",
      "@order(\"ignore\")",
      "string name;",
      "@order(\"descending\")",
      "Kind kind;",
      "MD5 hash;",
      "union { MD5, null} @aliases([\"hash\"]) nullableHash;",
      "array<long> arrayOfLongs;",
      "}"
    ]

main :: IO ()
main = hspec $ do
  describe "Parse annotations" $ do
    it "should parse namespaces" $ do
      parse parseNamespace "" "@namespace(\"mynamespace\")"
        `shouldParse` Namespace ["mynamespace"]
      parse parseNamespace "" "@namespace(\"org.apache.avro.test\")"
        `shouldParse` Namespace ["org", "apache", "avro", "test"]
    it "should parse ordering" $ do
      parse parseOrder "" "@order(\"ascending\")" `shouldParse` Ascending
      parse parseOrder "" "@order(\"descending\")" `shouldParse` Descending
      parse parseOrder "" "@order(\"ignore\")" `shouldParse` Ignore
    it "should parse aliases" $ do
      parse parseAliases "" "@aliases([\"org.foo.KindOf\"])"
        `shouldParse` [TN "KindOf" ["org", "foo"]]
      parse parseAliases "" "@aliases([\"org.old.OldRecord\", \"org.ancient.AncientRecord\"])"
        `shouldParse` [TN "OldRecord" ["org", "old"], TN "AncientRecord" ["org", "ancient"]]
    it "should parse other annotations" $ do
      parse parseAnnotation "" "@java-class(\"java.util.ArrayList\")"
        `shouldParse` Annotation "java-class" "java.util.ArrayList"
      parse parseAnnotation "" "@java-key-class(\"java.io.File\")"
        `shouldParse` Annotation "java-key-class" "java.io.File"
  describe "Parse imports" $ do
    it "should parse idl" $
      parse parseImport "" "import idl \"foo.avdl\";"
        `shouldParse` IdlImport "foo.avdl"
    it "should parse protocol" $
      parse parseImport "" "import protocol \"foo.avpr\";"
        `shouldParse` ProtocolImport "foo.avpr"
    it "should parse schema" $
      parse parseImport "" "import schema \"foo.avsc\";"
        `shouldParse` SchemaImport "foo.avsc"
  describe "Parse Data.Avro.Schema" $ do
    it "should parse null" $
      parse parseSchema "" "null" `shouldParse` Null
    it "should parse boolean" $
      parse parseSchema "" "boolean" `shouldParse` Boolean
    it "should parse int" $
      parse parseSchema "" "int" `shouldParse` Int'
    it "should parse long" $
      parse parseSchema "" "long" `shouldParse` Long'
    it "should parse float" $
      parse parseSchema "" "float" `shouldParse` Float
    it "should parse double" $
      parse parseSchema "" "double" `shouldParse` Double
    it "should parse decimal" $ do
      parse parseDecimal "" "decimal(4)" `shouldParse` Decimal 4 0
      parse parseDecimal "" "decimal(15,2)" `shouldParse` Decimal 15 2
    it "should parse date" $
      parse parseSchema "" "date" `shouldParse` Int (Just Date)
    it "should parse time" $
      parse parseSchema "" "time_ms" `shouldParse` Int (Just TimeMillis)
    it "should parse timestamp" $
      parse parseSchema "" "timestamp_ms" `shouldParse` Long (Just TimestampMillis)
    it "should parse bytes" $
      parse parseSchema "" "bytes" `shouldParse` Bytes'
    it "should parse string" $
      parse parseSchema "" "string" `shouldParse` String'
    it "should parse uuid" $
      parse parseSchema "" "uuid" `shouldParse` String (Just UUID)
    it "should parse array" $ do
      parse parseSchema "" "array<int>" `shouldParse` Array Int'
      parse parseSchema "" "array<array<string>>"
        `shouldParse` Array (Array String')
    it "should parse map" $ do
      parse parseSchema "" "map<int>" `shouldParse` Map Int'
      parse parseSchema "" "map<map<string>>"
        `shouldParse` Map (Map String')
    it "should parse unions" $
      parse parseSchema "" "union { string, int, null }"
        `shouldParse` Union (fromList [String', Int', Null])
    it "should parse fixeds" $ do
      parse parseSchema "" "fixed MD5(16)"
        `shouldParse` Fixed (TN "MD5" []) [] 16 Nothing
      parse parseSchema "" "@aliases([\"org.foo.MD5\"])\nfixed MD5(16)"
        `shouldParse` Fixed (TN "MD5" []) ["org.foo.MD5"] 16 Nothing
    it "should parse enums" $ do
      parse parseSchema "" enumTest
        `shouldParse` Enum (TN "Kind" []) [TN "KindOf" ["org", "foo"]] Nothing (fromList ["FOO", "BAR", "BAZ"])
      parse parseSchema "" "enum Suit { SPADES, DIAMONDS, CLUBS, HEARTS }"
        `shouldParse` Enum (TN "Suit" []) [] Nothing (fromList ["SPADES", "DIAMONDS", "CLUBS", "HEARTS"])
    it "should parse named types" $
      parse parseSchema "" "example.seed.server.protocol.avro.PeopleResponse"
        `shouldParse` NamedType
          ( TN
              { baseName = "PeopleResponse",
                namespace = ["example", "seed", "server", "protocol", "avro"]
              }
          )
    it "should parse simple records" $
      parse parseSchema "" simpleRecord
        `shouldParse` Record
          (TN "Person" [])
          [TN "Person" ["org", "foo"]]
          Nothing -- docs are ignored for now...
          [ Field "name" [] Nothing Nothing String' Nothing,
            Field "age" [] Nothing Nothing Int' Nothing,
            Field "birthday" [] Nothing Nothing (Int (Just Date)) Nothing
          ]
    it "should parse complex records" $
      parse parseSchema "" complexRecord
        `shouldParse` Record
          (TN "TestRecord" [])
          []
          Nothing -- docs are ignored for now...
          [ Field "name" [] Nothing (Just Ignore) String' Nothing,
            Field "kind" [] Nothing (Just Descending) (NamedType "Kind") Nothing,
            Field "hash" [] Nothing Nothing (NamedType "MD5") Nothing,
            Field "nullableHash" ["hash"] Nothing Nothing (Union $ fromList [NamedType "MD5", Null]) Nothing,
            Field "arrayOfLongs" [] Nothing Nothing (Array Long') Nothing
          ]
  describe "Parse protocols" $ do
    let getPerson =
          Method
            "getPerson"
            [Argument (NamedType "example.seed.server.protocol.avro.PeopleRequest") "request"]
            (NamedType "example.seed.server.protocol.avro.PeopleResponse")
            Null
            False
    it "should parse with imports" $
      parse parseProtocol "" (T.unlines . tail $ simpleProtocol)
        `shouldParse` Protocol Nothing "PeopleService" [IdlImport "People.avdl"] [] [getPerson]
    it "should parse with namespace" $
      parse parseProtocol "" (T.unlines simpleProtocol)
        `shouldParse` Protocol
          (Just (Namespace ["example", "seed", "server", "protocol", "avro"]))
          "PeopleService"
          [IdlImport "People.avdl"]
          []
          [getPerson]
  describe "Parse services" $ do
    it "should parse simple messages" $
      parse parseMethod "" "string hello(string greeting);"
        `shouldParse` Method "hello" [Argument String' "greeting"] String' Null False
    it "should parse more simple messages" $
      parse parseMethod "" "bytes echoBytes(bytes data);"
        `shouldParse` Method "echoBytes" [Argument Bytes' "data"] Bytes' Null False
    it "should parse custom type messages" $
      let custom = NamedType "TestRecord"
       in parse parseMethod "" "TestRecord echo(TestRecord `record`);"
            `shouldParse` Method "echo" [Argument custom "record"] custom Null False
    it "should parse multiple argument messages" $
      parse parseMethod "" "int add(int arg1, int arg2);"
        `shouldParse` Method "add" [Argument Int' "arg1", Argument Int' "arg2"] Int' Null False
    it "should parse escaped and throwing messages" $
      parse parseMethod "" "void `error`() throws TestError;"
        `shouldParse` Method "error" [] Null (NamedType $ TN "TestError" []) False
    it "should parse oneway messages" $
      parse parseMethod "" "void ping() oneway;"
        `shouldParse` Method "ping" [] Null Null True
