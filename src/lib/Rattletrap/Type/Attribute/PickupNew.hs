{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PickupNew where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupNewAttribute = PickupNewAttribute
  { instigatorId :: Maybe Word32le.Word32le
  , pickedUp :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''PickupNewAttribute)

bitPut :: PickupNewAttribute -> BitPut ()
bitPut pickupAttributeNew = do
  case instigatorId pickupAttributeNew of
    Nothing -> BinaryBits.putBool False
    Just instigatorId_ -> do
      BinaryBits.putBool True
      Word32le.bitPut instigatorId_
  Word8le.bitPut (pickedUp pickupAttributeNew)

bitGet :: BitGet PickupNewAttribute
bitGet = do
  instigator <- getBool
  PickupNewAttribute
    <$> decodeWhen instigator Word32le.bitGet
    <*> Word8le.bitGet
