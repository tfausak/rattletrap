module Rattletrap.Type.Attribute.Enum where

import qualified Data.Word as Word
import Prelude hiding (Enum)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype Enum = Enum
  { value :: Word.Word16
  } deriving (Eq, Show)

instance Json.FromValue Enum where
  fromValue = fmap Enum . Json.fromValue

instance Json.ToValue Enum where
  toValue = Json.toValue . value

schema :: Schema.Schema
schema = Schema.named "attribute-enum" $ Schema.ref Schema.integer

bitPut :: Enum -> BitPut.BitPut
bitPut enumAttribute = BitPut.bits 11 (value enumAttribute)

bitGet :: BitGet.BitGet Enum
bitGet = BitGet.label "Enum" $ do
  value <- BitGet.label "value" $ BitGet.bits 11
  pure Enum { value }
