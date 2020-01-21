{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Applicative hiding (some)
import Data.Text (Text)
import Data.Void
import qualified Language.Avro.Parser as P
import Language.Avro.Types
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main =
  hspec
    $ describe "Parse annotations"
    $ do
      it "should parse namespaces" $
        parse P.parseAnnotation "" "@namespace(\"mynamespace\")"
          `shouldBe` (Right $ Namespace "mynamespace")
      it "should parse ordering" $
        parse P.parseAnnotation "" "@order(\"ascending\")"
          `shouldBe` (Right $ Order "ascending")
      it "should parse aliases" $
        parse P.parseAnnotation "" "@aliases([\"org.old.OldRecord\", \"org.ancient.AncientRecord\"])"
          `shouldBe` (Right $ Aliases ["org.old.OldRecord", "org.ancient.AncientRecord"])
      it "should parse other annotations" $ do
        parse P.parseAnnotation "" "@java-class(\"java.util.ArrayList\")"
          `shouldBe` (Right $ OtherAnnotation "java-class" "java.util.ArrayList")
        parse P.parseAnnotation "" "@java-key-class(\"java.io.File\")"
          `shouldBe` (Right $ OtherAnnotation "java-key-class" "java.io.File")
