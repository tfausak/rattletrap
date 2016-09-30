module Rattletrap.AttributeValue where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data AttributeValue
  = BooleanAttribute
  | ByteAttribute
  | CamSettingsAttribute
  | DemolishAttribute
  | EnumAttribute
  | ExplosionAttribute
  | FlaggedIntAttribute
  | FloatAttribute
  | GameModeAttribute
  | IntAttribute
  | LoadoutAttribute
  | LoadoutOnlineAttribute
  | LoadoutsAttribute
  | LoadoutsOnlineAttribute
  | LocationAttribute
  | MusicStingerAttribute
  | PickupAttribute
  | PrivateMatchSettingsAttribute
  | QWordAttribute
  | RelativeRotationAttribute
  | ReservationAttribute
  | RigidBodyStateAttribute
  | StringAttribute
  | TeamPaintAttribute
  | UniqueIdAttribute
  | WeldedInfoAttribute
  deriving (Eq, Ord, Show)

getAttributeValue :: BinaryBit.BitGet AttributeValue
getAttributeValue = fail "getAttributeValue"

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue _ = fail "putAttributeValue"
