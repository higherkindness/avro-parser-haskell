{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Avro.Schema
import Data.Text as T hiding (tail)
import Data.Vector (fromList)
import Language.Avro.Parser
import Language.Avro.Types
import Test.Hspec
import Text.Megaparsec (parse)

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
    "}"
  ]

simpleRecord :: T.Text
simpleRecord =
  T.unlines
    [ "@aliases([\"org.foo.Person\"])",
      "record Person {",
      "string name;",
      "int age;",
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
        `shouldBe` (Right $ Namespace ["mynamespace"])
      parse parseNamespace "" "@namespace(\"org.apache.avro.test\")"
        `shouldBe` (Right $ Namespace ["org", "apache", "avro", "test"])
    it "should parse ordering" $ do
      parse parseOrder "" "@order(\"ascending\")" `shouldBe` Right Ascending
      parse parseOrder "" "@order(\"descending\")" `shouldBe` Right Descending
      parse parseOrder "" "@order(\"ignore\")" `shouldBe` Right Ignore
    it "should parse aliases" $ do
      parse parseAliases "" "@aliases([\"org.foo.KindOf\"])"
        `shouldBe` Right [TN "KindOf" ["org", "foo"]]
      parse parseAliases "" "@aliases([\"org.old.OldRecord\", \"org.ancient.AncientRecord\"])"
        `shouldBe` Right [TN "OldRecord" ["org", "old"], TN "AncientRecord" ["org", "ancient"]]
    it "should parse other annotations" $ do
      parse parseAnnotation "" "@java-class(\"java.util.ArrayList\")"
        `shouldBe` (Right $ Annotation "java-class" "java.util.ArrayList")
      parse parseAnnotation "" "@java-key-class(\"java.io.File\")"
        `shouldBe` (Right $ Annotation "java-key-class" "java.io.File")
  describe "Parse imports" $ do
    it "should parse idl" $
      parse parseImport "" "import idl \"foo.avdl\";"
        `shouldBe` (Right $ IdlImport "foo.avdl")
    it "should parse protocol" $
      parse parseImport "" "import protocol \"foo.avpr\";"
        `shouldBe` (Right $ ProtocolImport "foo.avpr")
    it "should parse schema" $
      parse parseImport "" "import schema \"foo.avsc\";"
        `shouldBe` (Right $ SchemaImport "foo.avsc")
  describe "Parse Data.Avro.Schema" $ do
    it "should parse null" $
      parse parseSchema "" "null" `shouldBe` Right Null
    it "should parse boolean" $
      parse parseSchema "" "boolean" `shouldBe` Right Boolean
    it "should parse int" $
      parse parseSchema "" "int" `shouldBe` Right Int
    it "should parse long" $
      parse parseSchema "" "long" `shouldBe` Right Long
    it "should parse float" $
      parse parseSchema "" "float" `shouldBe` Right Float
    it "should parse double" $
      parse parseSchema "" "double" `shouldBe` Right Double
    it "should parse bytes" $
      parse parseSchema "" "bytes" `shouldBe` Right Bytes
    it "should parse string" $
      parse parseSchema "" "string" `shouldBe` Right String
    it "should parse array" $ do
      parse parseSchema "" "array<int>" `shouldBe` (Right $ Array Int)
      parse parseSchema "" "array<array<string>>"
        `shouldBe` (Right $ Array $ Array String)
    it "should parse map" $ do
      parse parseSchema "" "map<int>" `shouldBe` (Right $ Map Int)
      parse parseSchema "" "map<map<string>>"
        `shouldBe` (Right $ Map $ Map String)
    it "should parse unions" $
      parse parseSchema "" "union { string, int, null }"
        `shouldBe` (Right $ Union $ fromList [String, Int, Null])
    it "should parse fixeds" $ do
      parse parseSchema "" "fixed MD5(16)"
        `shouldBe` (Right $ Fixed (TN "MD5" []) [] 16)
      parse parseSchema "" "@aliases([\"org.foo.MD5\"])\nfixed MD5(16)"
        `shouldBe` (Right $ Fixed (TN "MD5" []) ["org.foo.MD5"] 16)
    it "should parse enums" $ do
      parse parseSchema "" enumTest
        `shouldBe` (Right $ Enum (TN "Kind" []) [TN "KindOf" ["org", "foo"]] Nothing (fromList ["FOO", "BAR", "BAZ"]))
      parse parseSchema "" "enum Suit { SPADES, DIAMONDS, CLUBS, HEARTS }"
        `shouldBe` (Right $ Enum (TN "Suit" []) [] Nothing (fromList ["SPADES", "DIAMONDS", "CLUBS", "HEARTS"]))
    it "should parse named types" $
      parse parseSchema "" "example.seed.server.protocol.avro.PeopleResponse"
        `shouldBe` ( Right $ NamedType $ TN
                       { baseName = "PeopleResponse",
                         namespace = ["example", "seed", "server", "protocol", "avro"]
                       }
                   )
    it "should parse simple records" $
      parse parseSchema "" simpleRecord
        `shouldBe` ( Right $
                       Record
                         (TN "Person" [])
                         [TN "Person" ["org", "foo"]]
                         Nothing -- docs are ignored for now...
                         Nothing -- order is ignored for now...
                         [ Field "name" [] Nothing Nothing String Nothing,
                           Field "age" [] Nothing Nothing Int Nothing
                         ]
                   )
    it "should parse complex records" $
      parse parseSchema "" complexRecord
        `shouldBe` ( Right $
                       Record
                         (TN "TestRecord" [])
                         []
                         Nothing -- docs are ignored for now...
                         Nothing -- order is ignored for now...
                         [ Field "name" [] Nothing (Just Ignore) String Nothing,
                           Field "kind" [] Nothing (Just Descending) (NamedType "Kind") Nothing,
                           Field "hash" [] Nothing Nothing (NamedType "MD5") Nothing,
                           Field "nullableHash" ["hash"] Nothing Nothing (Union $ fromList [NamedType "MD5", Null]) Nothing,
                           Field "arrayOfLongs" [] Nothing Nothing (Array Long) Nothing
                         ]
                   )
  describe "Parse protocols" $ do
    it "should parse with imports" $
      parse parseProtocol "" (T.unlines . tail $ simpleProtocol)
        `shouldBe` ( Right $
                       Protocol
                         Nothing
                         "PeopleService"
                         [IdlImport "People.avdl"]
                   )
    it "should parse with namespace" $
      parse parseProtocol "" (T.unlines simpleProtocol)
        `shouldBe` ( Right $
                       Protocol
                         (Just (Namespace ["example", "seed", "server", "protocol", "avro"]))
                         "PeopleService"
                         [IdlImport "People.avdl"]
                   )
