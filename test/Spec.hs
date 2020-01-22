{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Avro.Schema
import Data.Vector (fromList)
import Language.Avro.Parser
import Language.Avro.Types
import Test.Hspec
import Text.Megaparsec (parse)

main :: IO ()
main = hspec $ do
  describe "Parse annotations" $ do
    it "should parse namespaces" $
      parse parseAnnotation "" "@namespace(\"mynamespace\")"
        `shouldBe` (Right $ Namespace "mynamespace")
    it "should parse ordering" $
      parse parseAnnotation "" "@order(\"ascending\")"
        `shouldBe` (Right $ Order "ascending")
    it "should parse aliases" $
      parse parseAnnotation "" "@aliases([\"org.old.OldRecord\", \"org.ancient.AncientRecord\"])"
        `shouldBe` (Right $ Aliases ["org.old.OldRecord", "org.ancient.AncientRecord"])
    it "should parse other annotations" $ do
      parse parseAnnotation "" "@java-class(\"java.util.ArrayList\")"
        `shouldBe` (Right $ OtherAnnotation "java-class" "java.util.ArrayList")
      parse parseAnnotation "" "@java-key-class(\"java.io.File\")"
        `shouldBe` (Right $ OtherAnnotation "java-key-class" "java.io.File")
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
      parse schemaType "" "null" `shouldBe` Right Null
    it "should parse boolean" $
      parse schemaType "" "boolean" `shouldBe` Right Boolean
    it "should parse int" $
      parse schemaType "" "int" `shouldBe` Right Int
    it "should parse long" $
      parse schemaType "" "long" `shouldBe` Right Long
    it "should parse float" $
      parse schemaType "" "float" `shouldBe` Right Float
    it "should parse double" $
      parse schemaType "" "double" `shouldBe` Right Double
    it "should parse bytes" $
      parse schemaType "" "bytes" `shouldBe` Right Bytes
    it "should parse string" $
      parse schemaType "" "string" `shouldBe` Right String
    it "should parse array" $ do
      parse schemaType "" "array<int>" `shouldBe` (Right $ Array Int)
      parse schemaType "" "array<array<string>>"
        `shouldBe` (Right $ Array $ Array String)
    it "should parse map" $ do
      parse schemaType "" "map<int>" `shouldBe` (Right $ Map Int)
      parse schemaType "" "map<map<string>>"
        `shouldBe` (Right $ Map $ Map String)
    it "should parse unions" $
      parse schemaType "" "union { string, int, null }"
        `shouldBe` (Right $ Union $ fromList [String, Int, Null])
    it "should parse named types" $
      parse schemaType "" "example.seed.server.protocol.avro.PeopleResponse"
        `shouldBe` ( Right $ NamedType $ TN
                       { baseName = "PeopleResponse",
                         namespace = ["example", "seed", "server", "protocol", "avro"]
                       }
                   )
