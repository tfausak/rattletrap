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
  , loadoutAttributeAntenna :: Word32
  , loadoutAttributeTopper :: Word32
  , loadoutAttributeUnknown1 :: Word32
  , loadoutAttributeUnknown2 :: Maybe Word32
  , loadoutAttributeUnknown3 :: Maybe Word32
  , loadoutAttributeUnknown4 :: Maybe Word32
  , loadoutAttributeUnknown5 :: Maybe Word32
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
  unknown3 <- getOptional (version >= Word8 16) getWord32Bits
  unknown4 <- getOptional (version >= Word8 16) getWord32Bits
  unknown5 <- getOptional (version >= Word8 16) getWord32Bits
  pure
    (LoadoutAttribute
       version
       body
       decal
       wheels
       rocketTrail
       antenna
       topper
       unknown1
       unknown2
       unknown3
       unknown4
       unknown5)

getOptional :: Bool -> BinaryBit.BitGet a -> BinaryBit.BitGet (Maybe a)
getOptional p f =
  if p
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
  putOptional (loadoutAttributeUnknown3 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeUnknown4 loadoutAttribute) putWord32Bits
  putOptional (loadoutAttributeUnknown5 loadoutAttribute) putWord32Bits

putOptional :: Maybe a -> (a -> BinaryBit.BitPut ()) -> BinaryBit.BitPut ()
putOptional m f =
  case m of
    Just x -> f x
    Nothing -> pure ()
