module Rattletrap.Type.Common
  ( deriveJson
  , jsonOptions
  ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.TH as Json
import qualified Language.Haskell.TH as TH

deriveJson :: TH.Name -> TH.Q [TH.Dec]
deriveJson = Json.deriveJSON jsonOptions

jsonOptions :: Json.Options
jsonOptions = Json.defaultOptions
  { Json.constructorTagModifier = toSnakeCase
  , Json.fieldLabelModifier = toSnakeCase
  , Json.omitNothingFields = True
  , Json.sumEncoding = Json.ObjectWithSingleField
  , Json.unwrapUnaryRecords = True
  }

toSnakeCase :: String -> String
toSnakeCase = Json.camelTo2 '_'
