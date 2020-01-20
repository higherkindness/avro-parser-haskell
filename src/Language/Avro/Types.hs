module Language.Avro.Types where

import qualified Data.Text as T

data TypeName
  = TN
      { baseName :: T.Text,
        namespace :: [T.Text]
      }

data ImportType
  = IdlImport
  | ProtocolImport
  | SchemaImport
  deriving (Eq, Show)
