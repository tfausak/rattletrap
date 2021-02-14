{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Pickup where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

data Pickup = Pickup
  { instigatorId :: Maybe U32.U32
  , pickedUp :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''Pickup)

bitPut :: Pickup -> BitPut.BitPut
bitPut pickupAttribute = do
  case instigatorId pickupAttribute of
    Nothing -> BitPut.bool False
    Just instigatorId_ -> do
      BitPut.bool True
      U32.bitPut instigatorId_
  BitPut.bool (pickedUp pickupAttribute)

bitGet :: BitGet.BitGet Pickup
bitGet = do
  instigator <- BitGet.bool
  Pickup <$> decodeWhen instigator U32.bitGet <*> BitGet.bool
