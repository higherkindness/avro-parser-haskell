module Language.Avro.Types (
  module Language.Avro.Types, Schema (..)
) where

import Data.Avro.Schema
import qualified Data.Text as T

data Protocol
  = Protocol
      { ns :: Maybe Namespace,
        pname :: T.Text,
        imports :: [ImportType],
        types :: [Schema],
        messages :: [Method]
      }
  deriving (Eq, Show)

instance Semigroup Protocol where
  p1 <> p2 =
    Protocol
      (ns p1)
      (pname p1)
      (imports p1 <> imports p2)
      (types p1 <> types p2)
      (messages p1 <> messages p2)

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

data Argument
  = Argument
      { atype :: Schema,
        aname :: T.Text
      }
  deriving (Eq, Show)

data Method
  = Method
      { mname :: T.Text,
        args :: [Argument],
        result :: Schema,
        throws :: Schema,
        oneway :: Bool
      }
  deriving (Eq, Show)
