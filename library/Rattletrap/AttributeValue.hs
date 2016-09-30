module Rattletrap.AttributeValue where

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data AttributeValue
  = BooleanAttribute Bool
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

getAttributeValue :: String -> BinaryBit.BitGet AttributeValue
getAttributeValue name =
  case name of
    "Engine.Actor:bBlockActors" -> getBooleanAttribute
    _ -> fail ("getAttributeValue: " ++ show name)

getBooleanAttribute :: BinaryBit.BitGet AttributeValue
getBooleanAttribute = do
  x <- BinaryBit.getBool
  pure (BooleanAttribute x)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> BinaryBit.putBool x
    _ -> fail "putAttributeValue"
