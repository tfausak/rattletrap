{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PickupNew where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PickupNewAttribute = PickupNewAttribute
  { instigatorId :: Maybe U32.U32
  , pickedUp :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''PickupNewAttribute)

bitPut :: PickupNewAttribute -> BitPut ()
bitPut pickupAttributeNew = do
  case instigatorId pickupAttributeNew of
    Nothing -> BinaryBits.putBool False
    Just instigatorId_ -> do
      BinaryBits.putBool True
      U32.bitPut instigatorId_
  U8.bitPut (pickedUp pickupAttributeNew)

bitGet :: BitGet PickupNewAttribute
bitGet = do
  instigator <- getBool
  PickupNewAttribute
    <$> decodeWhen instigator U32.bitGet
    <*> U8.bitGet
