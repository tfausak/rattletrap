module Rattletrap.AttributeValue.Loadout where

import Rattletrap.Word32
import Rattletrap.Word8

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data LoadoutAttributeValue = LoadoutAttributeValue
  { loadoutAttributeValueVersion :: Word8
  , loadoutAttributeValueBody :: Word32
  , loadoutAttributeValueDecal :: Word32
  , loadoutAttributeValueWheels :: Word32
  , loadoutAttributeValueRocketTrail :: Word32
  , loadoutAttributeValueAntenna :: Word32
  , loadoutAttributeValueTopper :: Word32
  , loadoutAttributeValueUnknown1 :: Word32
  , loadoutAttributeValueUnknown2 :: Maybe Word32
  } deriving (Eq, Ord, Show)

getLoadoutAttributeValue :: BinaryBit.BitGet LoadoutAttributeValue
getLoadoutAttributeValue = do
  version <- getWord8Bits
  body <- getWord32Bits
  decal <- getWord32Bits
  wheels <- getWord32Bits
  rocketTrail <- getWord32Bits
  antenna <- getWord32Bits
  topper <- getWord32Bits
  g <- getWord32Bits
  h <-
    if version > Word8 10
      then do
        h <- getWord32Bits
        pure (Just h)
      else pure Nothing
  pure
    (LoadoutAttributeValue
       version
       body
       decal
       wheels
       rocketTrail
       antenna
       topper
       g
       h)

putLoadoutAttributeValue :: LoadoutAttributeValue -> BinaryBit.BitPut ()
putLoadoutAttributeValue loadoutAttributeValue = do
  putWord8Bits (loadoutAttributeValueVersion loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueBody loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueDecal loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueWheels loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueRocketTrail loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueAntenna loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueTopper loadoutAttributeValue)
  putWord32Bits (loadoutAttributeValueUnknown1 loadoutAttributeValue)
  case loadoutAttributeValueUnknown2 loadoutAttributeValue of
    Nothing -> pure ()
    Just x -> putWord32Bits x
