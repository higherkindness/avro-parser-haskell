{-# LANGUAGE StandaloneDeriving #-}

-- | Language definition for AVRO (@.avdl@) files,
--   as defined in <http://avro.apache.org/docs/1.8.2/spec.html>.
module Language.Avro.Types
  ( module Language.Avro.Types,
    Schema (..),
  )
where

import Data.Avro.Schema
import Data.Avro.Types.Value
import Data.Set
import qualified Data.Text as T

-- | Whole definition of protocol.
data Protocol
  = Protocol
      { ns :: Maybe Namespace,
        pname :: T.Text,
        imports :: Set ImportType,
        types :: Set Schema,
        messages :: Set Method
      }
  deriving (Eq, Show, Ord)

instance Semigroup Protocol where
  p1 <> p2 =
    Protocol
      (ns p1)
      (pname p1)
      (imports p1 <> imports p2)
      (types p1 <> types p2)
      (messages p1 <> messages p2)

deriving instance Ord LogicalTypeInt

deriving instance Ord LogicalTypeLong

deriving instance Ord LogicalTypeBytes

deriving instance Ord Decimal

deriving instance Ord LogicalTypeString

deriving instance Ord LogicalTypeFixed

deriving instance (Ord a) => Ord (Value a)

deriving instance Ord Field

deriving instance Ord Schema

-- | Newtype for the namespace of methods and protocols.
newtype Namespace
  = Namespace [T.Text]
  deriving (Eq, Show, Ord)

type Aliases = [TypeName]

-- | Type for special annotations.
data Annotation
  = Annotation
      { ann :: T.Text,
        abody :: T.Text
      }
  deriving (Eq, Show)

-- | Type for the possible import types in 'Protocol'.
data ImportType
  = IdlImport T.Text
  | ProtocolImport T.Text
  | SchemaImport T.Text
  deriving (Eq, Show, Ord)

-- | Helper type for the arguments of 'Method'.
data Argument
  = Argument
      { atype :: Schema,
        aname :: T.Text
      }
  deriving (Eq, Show, Ord)

-- | Type for methods/messages that belong to a protocol.
data Method
  = Method
      { mname :: T.Text,
        args :: [Argument],
        result :: Schema,
        throws :: Schema,
        oneway :: Bool
      }
  deriving (Eq, Show, Ord)
