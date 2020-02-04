# avro-parser-haskell

[![Actions Status](https://github.com/kutyel/avro-parser-haskell/workflows/Haskell%20CI/badge.svg)](https://github.com/kutyel/avro-parser-haskell/actions)
[![ormolu](https://img.shields.io/badge/styled%20with-ormolu-blueviolet)](https://github.com/tweag/ormolu)

Language definition and parser for AVRO (`.avdl`) files.

## Example

```haskell
#! /usr/bin/env runhaskell

module Main where

import Language.Avro.Parser (readWithImports)

main :: IO ()
main = readWithImports "test" "PeopleService.avdl"
-- λ>
-- Right
--   (Protocol {
--     ns = Just (Namespace ["example","seed","server","protocol","avro"]),
--     pname = "PeopleService", imports = [IdlImport "People.avdl"],
--     types = [
--       Record {
--         name = "Person",
--         aliases = [],
--         doc = Nothing,
--         order = Nothing,
--         fields = [
--           Field {
--             fldName = "name",
--             fldAliases = [],
--             fldDoc = Nothing,
--             fldOrder = Nothing,
--             fldType = String,
--             fldDefault = Nothing
--           },
--           Field {
--             fldName = "age",
--             fldAliases = [],
--             fldDoc = Nothing,
--             fldOrder = Nothing,
--             fldType = Int,
--             fldDefault = Nothing
--           }
--         ]
--       },
--       Record {
--         name = "NotFoundError",
--         aliases = [],
--         doc = Nothing,
--         order = Nothing,
--         fields = [
--           Field {
--             fldName = "message",
--             fldAliases = [],
--             fldDoc = Nothing,
--             fldOrder = Nothing,
--             fldType = String,
--             fldDefault = Nothing
--           }
--         ]
--       },
--       Record {
--         name = "DuplicatedPersonError",
--         aliases = [],
--         doc = Nothing,
--         order = Nothing,
--         fields = [
--           Field {
--             fldName = "message",
--             fldAliases = [],
--             fldDoc = Nothing,
--             fldOrder = Nothing,
--             fldType = String,
--             fldDefault = Nothing
--           }
--         ]
--       },
--       Record {
--         name = "PeopleRequest",
--         aliases = [],
--         doc = Nothing,
--         order = Nothing,
--         fields = [
--           Field {
--             fldName = "name",
--             fldAliases = [],
--             fldDoc = Nothing,
--             fldOrder = Nothing,
--             fldType = String,
--             fldDefault = Nothing
--           }
--         ]
--       },
--       Record {
--         name = "PeopleResponse",
--         aliases = [],
--         doc = Nothing,
--         order = Nothing,
--         fields = [
--           Field {
--             fldName = "result",
--             fldAliases = [],
--             fldDoc = Nothing,
--             fldOrder = Nothing,
--             fldType = Union {options = [NamedType "Person",NamedType "NotFoundError",NamedType "DuplicatedPersonError"]},
--             fldDefault = Nothing
--           }
--         ]
--       }
--     ],
--     messages = [
--       Method {
--         mname = "getPerson",
--         args = [Argument {atype = NamedType "example.seed.server.protocol.avro.PeopleRequest", aname = "request"}],
--         result = NamedType "example.seed.server.protocol.avro.PeopleResponse",
--         throws = Null,
--         oneway = False
--       }
--     ]
--   })
```

⚠️ Warning: `readWithImports` only works right know if the import type is `"idl"`!
