{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Language.Avro.Parser
import Language.Avro.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main =
  hspec
    $ describe "Parse annotations"
    $ do
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
