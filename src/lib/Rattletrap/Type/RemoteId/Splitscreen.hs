module Rattletrap.Type.RemoteId.Splitscreen where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

newtype Splitscreen
  = Splitscreen Word.Word32
  deriving (Eq, Show)

instance Json.FromValue Splitscreen where
  fromValue = fmap fromWord32 . Json.fromValue

instance Json.ToValue Splitscreen where
  toValue = Json.toValue . toWord32

fromWord32 :: Word.Word32 -> Splitscreen
fromWord32 = Splitscreen

toWord32 :: Splitscreen -> Word.Word32
toWord32 (Splitscreen x) = x

schema :: Schema.Schema
schema = Schema.integer

bitPut :: Splitscreen -> BitPut.BitPut
bitPut = BitPut.bits 24 . toWord32

bitGet :: BitGet.BitGet Splitscreen
bitGet = BitGet.label "Splitscreen" . fmap fromWord32 $ BitGet.bits 24
