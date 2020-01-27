module Language.Avro.Types where

import Data.Avro.Schema
import qualified Data.Text as T

data Protocol
  = Protocol
      { ns :: Maybe Namespace,
        pname :: T.Text,
        imports :: [ImportType]
        -- TODO: , types :: [Schema]
        -- TODO: , messages :: [Method]
      }
  deriving (Eq, Show)

newtype Namespace
  = Namespace [T.Text]
  deriving (Eq, Show)

type Aliases = [TypeName]

data Annotation
  = Annotation
      { ann :: T.Text,
        body :: T.Text
      }
  deriving (Eq, Show)

data ImportType
  = IdlImport T.Text
  | ProtocolImport T.Text
  | SchemaImport T.Text
  deriving (Eq, Show)

data Method
  = Method
      { mname :: T.Text,
        args :: [Schema],
        result :: Schema,
        throws :: Schema,
        oneway :: Bool
      }
  deriving (Eq, Show)
