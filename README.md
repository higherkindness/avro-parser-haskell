# avro-parser-haskell

[![Actions Status](https://github.com/higherkindness/avro-parser-haskell/workflows/Haskell%20CI/badge.svg)](https://github.com/higherkindness/avro-parser-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/language-avro.svg?logo=haskell)](https://hackage.haskell.org/package/language-avro)
[![Stackage Nightly](http://stackage.org/package/language-avro/badge/nightly)](http://stackage.org/nightly/package/language-avro)
[![Stackage LTS](http://stackage.org/package/language-avro/badge/lts)](http://stackage.org/lts/package/language-avro)
![Hackage-Deps](https://img.shields.io/hackage-deps/v/language-avro?style=flat)
[![ormolu](https://img.shields.io/badge/styled%20with-ormolu-blueviolet)](https://github.com/tweag/ormolu)

Language definition and parser for AVRO (`.avdl`) files.

## Example

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-18.19 script --package language-avro,pretty-simple

module Main where

import Language.Avro.Parser (readWithImports)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main =
  readWithImports "test" "PeopleService.avdl"
    >>= either putStrLn pPrint
-- λ>
-- Protocol
--   { ns = Just
--       ( Namespace
--           [ "example"
--           , "seed"
--           , "server"
--           , "protocol"
--           , "avro"
--           ]
--       )
--   , pname = "PeopleService"
--   , imports = [ IdlImport "People.avdl" ]
--   , types =
--       [ Record
--           { name = "Person"
--           , aliases = []
--           , doc = Nothing
--           , order = Nothing
--           , fields =
--               [ Field
--                   { fldName = "name"
--                   , fldAliases = []
--                   , fldDoc = Nothing
--                   , fldOrder = Nothing
--                   , fldType = String { logicalTypeS = Nothing }
--                   , fldDefault = Nothing
--                   }
--               , Field
--                   { fldName = "age"
--                   , fldAliases = []
--                   , fldDoc = Nothing
--                   , fldOrder = Nothing
--                   , fldType = Int { logicalTypeI = Nothing }
--                   , fldDefault = Nothing
--                   }
--               ]
--           }
--       , Record
--           { name = "NotFoundError"
--           , aliases = []
--           , doc = Nothing
--           , order = Nothing
--           , fields =
--               [ Field
--                   { fldName = "message"
--                   , fldAliases = []
--                   , fldDoc = Nothing
--                   , fldOrder = Nothing
--                   , fldType = String { logicalTypeS = Nothing }
--                   , fldDefault = Nothing
--                   }
--               ]
--           }
--       , Record
--           { name = "DuplicatedPersonError"
--           , aliases = []
--           , doc = Nothing
--           , order = Nothing
--           , fields =
--               [ Field
--                   { fldName = "message"
--                   , fldAliases = []
--                   , fldDoc = Nothing
--                   , fldOrder = Nothing
--                   , fldType = String { logicalTypeS = Nothing }
--                   , fldDefault = Nothing
--                   }
--               ]
--           }
--       , Record
--           { name = "PeopleRequest"
--           , aliases = []
--           , doc = Nothing
--           , order = Nothing
--           , fields =
--               [ Field
--                   { fldName = "name"
--                   , fldAliases = []
--                   , fldDoc = Nothing
--                   , fldOrder = Nothing
--                   , fldType = String { logicalTypeS = Nothing }
--                   , fldDefault = Nothing
--                   }
--               ]
--           }
--       , Record
--           { name = "PeopleResponse"
--           , aliases = []
--           , doc = Nothing
--           , order = Nothing
--           , fields =
--               [ Field
--                   { fldName = "result"
--                   , fldAliases = []
--                   , fldDoc = Nothing
--                   , fldOrder = Nothing
--                   , fldType = Union
--                       { options =
--                           [ NamedType "Person"
--                           , NamedType "NotFoundError"
--                           , NamedType "DuplicatedPersonError"
--                           ]
--                       }
--                   , fldDefault = Nothing
--                   }
--               ]
--           }
--       ]
--   , messages =
--       [ Method
--           { mname = "getPerson"
--           , args =
--               [ Argument
--                   { atype = NamedType "example.seed.server.protocol.avro.PeopleRequest"
--                   , aname = "request"
--                   }
--               ]
--           , result = NamedType "example.seed.server.protocol.avro.PeopleResponse"
--           , throws = Null
--           , oneway = False
--           }
--       ]
--   }
```

⚠️ Warning: `readWithImports` only works right now if the import type is `"idl"`!
