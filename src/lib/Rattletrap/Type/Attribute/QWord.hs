module Rattletrap.Type.Attribute.QWord where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Utility.Json as Json

newtype QWord = QWord
  { value :: U64.U64
  } deriving (Eq, Show)

instance Json.FromValue QWord where
  fromValue = fmap QWord . Json.fromValue

instance Json.ToValue QWord where
  toValue = Json.toValue . value

schema :: Schema.Schema
schema = Schema.named "attribute-q-word" $ Schema.ref U64.schema

bitPut :: QWord -> BitPut.BitPut
bitPut qWordAttribute = U64.bitPut (value qWordAttribute)

bitGet :: BitGet.BitGet QWord
bitGet = BitGet.label "QWord" $ do
  value <- BitGet.label "value" U64.bitGet
  pure QWord { value }
