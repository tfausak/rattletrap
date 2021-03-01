module Rattletrap.Type.Attribute.Byte where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

newtype Byte = Byte
  { value :: U8.U8
  } deriving (Eq, Show)

instance Json.FromJSON Byte where
  parseJSON = fmap Byte . Json.parseJSON

instance Json.ToJSON Byte where
  toJSON = Json.toJSON . value

schema :: Schema.Schema
schema = Schema.named "attribute-byte" $ Schema.ref U8.schema

bitPut :: Byte -> BitPut.BitPut
bitPut byteAttribute = U8.bitPut (value byteAttribute)

bitGet :: BitGet.BitGet Byte
bitGet = Byte <$> U8.bitGet
