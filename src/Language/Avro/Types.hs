-- | Language definition for AVRO (@.avdl@) files,
--   as defined in <http://avro.apache.org/docs/1.8.2/spec.html>.
module Language.Avro.Types (
  module Language.Avro.Types, Schema (..)
) where

import Data.Avro.Schema
import qualified Data.Text as T

-- | Whole definition of protocol.
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

-- | Newtype for the namespace of methods and protocols.
newtype Namespace
  = Namespace [T.Text]
  deriving (Eq, Show)

type Aliases = [TypeName]

-- | Type for special annotations.
data Annotation
  = Annotation
      { ann :: T.Text,
        abody :: T.Text
      }
  deriving (Eq, Show)

-- | Type for the possible import types in @Protocol@.
data ImportType
  = IdlImport T.Text
  | ProtocolImport T.Text
  | SchemaImport T.Text
  deriving (Eq, Show)

-- | Helper type for the arguments of @Method@.
data Argument
  = Argument
      { atype :: Schema,
        aname :: T.Text
      }
  deriving (Eq, Show)

-- | Type for methods/messages that belong to a protocol.
data Method
  = Method
      { mname :: T.Text,
        args :: [Argument],
        result :: Schema,
        throws :: Schema,
        oneway :: Bool
      }
  deriving (Eq, Show)
