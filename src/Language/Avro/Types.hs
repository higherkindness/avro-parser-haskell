module Language.Avro.Types where

import qualified Data.Text as T

data Annotation
  = Namespace T.Text
  | Order T.Text
  | Aliases [T.Text]
  | OtherAnnotation
      { ann :: T.Text,
        body :: T.Text
      }
  deriving (Eq, Show)

data ImportType
  = IdlImport T.Text
  | ProtocolImport T.Text
  | SchemaImport T.Text
  deriving (Eq, Show)
