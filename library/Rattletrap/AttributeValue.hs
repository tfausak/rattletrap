module Rattletrap.AttributeValue where

import Rattletrap.Int32
import Rattletrap.Location
import Rattletrap.Spin
import Rattletrap.Text
import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data AttributeValue
  = BooleanAttribute Bool
  | ByteAttribute Word8
  | CamSettingsAttribute
  | DemolishAttribute
  | EnumAttribute
  | ExplosionAttribute
  | FlaggedIntAttribute Bool
                        Int32
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
  | RigidBodyStateAttribute Bool
                            Location
                            Spin
                            (Maybe Location)
                            (Maybe Location)
  | StringAttribute
  | TeamPaintAttribute
  | UniqueIdAttribute
  | WeldedInfoAttribute
  deriving (Eq, Ord, Show)

getAttributeValue :: Text -> BinaryBit.BitGet AttributeValue
getAttributeValue name =
  case textToString name of
    "Engine.Actor:bBlockActors" -> getBooleanAttribute
    "Engine.PlayerReplicationInfo:Ping" -> getByteAttribute
    "TAGame.Ball_TA:GameEvent" -> getFlaggedIntAttribute
    "TAGame.RBActor_TA:ReplicatedRBState" -> getRigidBodyStateAttribute
    _ -> fail ("don't know how to get attribute value " ++ show name)

getBooleanAttribute :: BinaryBit.BitGet AttributeValue
getBooleanAttribute = do
  x <- BinaryBit.getBool
  pure (BooleanAttribute x)

getByteAttribute :: BinaryBit.BitGet AttributeValue
getByteAttribute = do
  byte <- getWord8Bits
  pure (ByteAttribute byte)

getFlaggedIntAttribute :: BinaryBit.BitGet AttributeValue
getFlaggedIntAttribute = do
  flag <- BinaryBit.getBool
  int <- getInt32Bits
  pure (FlaggedIntAttribute flag int)

getRigidBodyStateAttribute :: BinaryBit.BitGet AttributeValue
getRigidBodyStateAttribute = do
  isSleeping <- BinaryBit.getBool
  location <- getLocation
  spin <- getSpin
  linearVelocity <-
    if isSleeping
      then pure Nothing
      else do
        linearVelocity <- getLocation
        pure (Just linearVelocity)
  angularVelocity <-
    if isSleeping
      then pure Nothing
      else do
        angularVelocity <- getLocation
        pure (Just angularVelocity)
  pure
    (RigidBodyStateAttribute
       isSleeping
       location
       spin
       linearVelocity
       angularVelocity)

putAttributeValue :: AttributeValue -> BinaryBit.BitPut ()
putAttributeValue value =
  case value of
    BooleanAttribute x -> BinaryBit.putBool x
    ByteAttribute byte -> putWord8Bits byte
    FlaggedIntAttribute flag int -> do
      BinaryBit.putBool flag
      putInt32Bits int
    RigidBodyStateAttribute isSleeping location spin maybeLinearVelocity maybeAngularVelocity -> do
      BinaryBit.putBool isSleeping
      putLocation location
      putSpin spin
      case maybeLinearVelocity of
        Nothing -> pure ()
        Just linearVelocity -> putLocation linearVelocity
      case maybeAngularVelocity of
        Nothing -> pure ()
        Just angularVelocity -> putLocation angularVelocity
    _ -> fail ("don't know how to put attribute value " ++ show value)
