{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PickupAttributeNew where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupAttributeNew = PickupAttributeNew
  { pickupAttributeNewInstigatorId :: Maybe Word32le
  , pickupAttributeNewPickedUp :: Word8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''PickupAttributeNew)

putPickupAttributeNew :: PickupAttributeNew -> BinaryBits.BitPut ()
putPickupAttributeNew pickupAttributeNew = do
  case pickupAttributeNewInstigatorId pickupAttributeNew of
    Nothing -> BinaryBits.putBool False
    Just instigatorId -> do
      BinaryBits.putBool True
      putWord32Bits instigatorId
  putWord8Bits (pickupAttributeNewPickedUp pickupAttributeNew)

decodePickupAttributeNewBits :: DecodeBits PickupAttributeNew
decodePickupAttributeNewBits = do
  instigator <- getBool
  PickupAttributeNew
    <$> decodeWhen instigator decodeWord32leBits
    <*> decodeWord8leBits
