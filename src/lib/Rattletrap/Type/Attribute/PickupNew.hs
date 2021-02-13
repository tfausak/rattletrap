{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PickupNew where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupAttributeNew = PickupAttributeNew
  { pickupAttributeNewInstigatorId :: Maybe Word32le
  , pickupAttributeNewPickedUp :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''PickupAttributeNew)

putPickupAttributeNew :: PickupAttributeNew -> BitPut ()
putPickupAttributeNew pickupAttributeNew = do
  case pickupAttributeNewInstigatorId pickupAttributeNew of
    Nothing -> BinaryBits.putBool False
    Just instigatorId -> do
      BinaryBits.putBool True
      putWord32Bits instigatorId
  Word8le.bitPut (pickupAttributeNewPickedUp pickupAttributeNew)

decodePickupAttributeNewBits :: BitGet PickupAttributeNew
decodePickupAttributeNewBits = do
  instigator <- getBool
  PickupAttributeNew
    <$> decodeWhen instigator decodeWord32leBits
    <*> Word8le.bitGet
