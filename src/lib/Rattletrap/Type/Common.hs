module Rattletrap.Type.Common
  ( Int.Int8
  , Int.Int32
  , Int.Int64
  , Map.Map
  , Text.Text
  , Word.Word8
  , Word.Word16
  , Word.Word32
  , Word.Word64
  , deriveJson
  , jsonOptions
  ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.TH as Json
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Word as Word
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
