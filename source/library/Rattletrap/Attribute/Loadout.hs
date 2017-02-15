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
  } deriving (Eq, Show)

getLoadoutAttribute :: BinaryBit.BitGet LoadoutAttribute
getLoadoutAttribute = do
  version <- getWord8Bits
  body <- getWord32Bits
  decal <- getWord32Bits
  wheels <- getWord32Bits
  rocketTrail <- getWord32Bits
  antenna <- getWord32Bits
  topper <- getWord32Bits
  g <- getWord32Bits
  h <-
    if word8Value version > 10
      then do
        h <- getWord32Bits
        pure (Just h)
      else pure Nothing
  pure
    (LoadoutAttribute version body decal wheels rocketTrail antenna topper g h)

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
  case loadoutAttributeUnknown2 loadoutAttribute of
    Nothing -> pure ()
    Just x -> putWord32Bits x
