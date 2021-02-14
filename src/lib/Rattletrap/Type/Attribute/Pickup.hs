{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Pickup where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupAttribute = PickupAttribute
  { instigatorId :: Maybe Word32le.Word32le
  , pickedUp :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''PickupAttribute)

bitPut :: PickupAttribute -> BitPut ()
bitPut pickupAttribute = do
  case instigatorId pickupAttribute of
    Nothing -> BinaryBits.putBool False
    Just instigatorId_ -> do
      BinaryBits.putBool True
      Word32le.bitPut instigatorId_
  BinaryBits.putBool (pickedUp pickupAttribute)

bitGet :: BitGet PickupAttribute
bitGet = do
  instigator <- getBool
  PickupAttribute <$> decodeWhen instigator Word32le.bitGet <*> getBool
