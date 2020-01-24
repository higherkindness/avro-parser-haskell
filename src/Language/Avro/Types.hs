module Language.Avro.Types where

import Data.Avro.Schema
import qualified Data.Text as T

data Protocol
  = Protocol
      { name :: T.Text,
        imports :: [ImportType]
        -- TODO: , types :: [Schema]
        -- TODO: , services :: [ServiceDecl]
      }
  deriving (Eq, Show)

type Aliases = [TypeName]

data Annotation
  = Namespace T.Text
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
