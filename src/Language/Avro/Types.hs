module Language.Avro.Types where

data ImportType
  = IdlImport
  | ProtocolImport
  | SchemaImport
  deriving (Eq, Show)
