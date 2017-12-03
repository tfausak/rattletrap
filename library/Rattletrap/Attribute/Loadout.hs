module Rattletrap.Attribute.Loadout where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data LoadoutAttribute = LoadoutAttribute
  { loadoutAttributeVersion :: Word8
  , loadoutAttributeBody :: Word32
  , loadoutAttributeDecal :: Word32
  , loadoutAttributeWheels :: Word32
  , loadoutAttributeRocketTrail :: Word32
  -- ^ Now known as "rocket boost".
  , loadoutAttributeAntenna :: Word32
  , loadoutAttributeTopper :: Word32
  , loadoutAttributeUnknown1 :: Word32
  , loadoutAttributeUnknown2 :: Maybe Word32
  , loadoutAttributeEngineAudio :: Maybe Word32
  , loadoutAttributeTrail :: Maybe Word32
  , loadoutAttributeGoalExplosion :: Maybe Word32
  , loadoutAttributeBanner :: Maybe Word32
  } deriving (Eq, Ord, Show)

getLoadoutAttribute :: BinaryBit.BitGet LoadoutAttribute
getLoadoutAttribute = do
  version <- getWord8Bits
  body <- getWord32Bits
  decal <- getWord32Bits
  wheels <- getWord32Bits
  rocketTrail <- getWord32Bits
  antenna <- getWord32Bits
  topper <- getWord32Bits
  unknown1 <- getWord32Bits
  unknown2 <- getOptional (version > Word8 10) getWord32Bits
  engineAudio <- getOptional (version >= Word8 16) getWord32Bits
  trail <- getOptional (version >= Word8 16) getWord32Bits
  goalExplosion <- getOptional (version >= Word8 16) getWord32Bits
  banner <- getOptional (version >= Word8 17) getWord32Bits
  pure
    ( LoadoutAttribute
      version
      body
      decal
      wheels
      rocketTrail
      antenna
      topper
      unknown1
      unknown2
      engineAudio
      trail
      goalExplosion
      banner
    )

getOptional :: Bool -> BinaryBit.BitGet a -> BinaryBit.BitGet (Maybe a)
getOptional p f = if p
  then do
    x <- f
    pure (Just x)
  else pure Nothing

putLoadoutAttribute :: LoadoutAttribute -> BinaryBit.BitPut ()
putLoadoutAttribute loadoutAttribute = do
  putWord8Bits (loadoutAttributeVersion loadoutAttribute)
  putWord32Bits (loadoutAttributeBody loadoutAttribute)
  putWord32Bits (loadoutAttributeDecal loadoutAttribute)
  putWord32Bits (loadoutAttributeWheels loadoutAttribute)
  putWord32Bits (loadoutAttributeRocketTrail loadoutAttribute)
  putWord32Bits (loadoutAttributeAntenna loadoutAttribute)
  putWord32Bits (loadoutAttributeTopper loadoutAttribute)
  putWord32Bits (loadoutAttributeUnknown1 loadoutAttribute)
  putOptional (loadoutAttributeUnknown2 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeEngineAudio loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeTrail loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeGoalExplosion loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeBanner loadoutAttribute) putWord32Bits

putOptional :: Maybe a -> (a -> BinaryBit.BitPut ()) -> BinaryBit.BitPut ()
putOptional m f = case m of
  Just x -> f x
  Nothing -> pure ()
