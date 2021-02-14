{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Pickup where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupAttribute = PickupAttribute
  { instigatorId :: Maybe U32.U32
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
      U32.bitPut instigatorId_
  BinaryBits.putBool (pickedUp pickupAttribute)

bitGet :: BitGet PickupAttribute
bitGet = do
  instigator <- getBool
  PickupAttribute <$> decodeWhen instigator U32.bitGet <*> getBool
