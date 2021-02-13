{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FlaggedByteAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedByteAttribute = FlaggedByteAttribute
  { flaggedByteAttributeFlag :: Bool
  , flaggedByteAttributeByte :: Word8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''FlaggedByteAttribute)

putFlaggedByteAttribute :: FlaggedByteAttribute -> BinaryBits.BitPut ()
putFlaggedByteAttribute flaggedByteAttribute = do
  BinaryBits.putBool (flaggedByteAttributeFlag flaggedByteAttribute)
  putWord8Bits (flaggedByteAttributeByte flaggedByteAttribute)

decodeFlaggedByteAttributeBits :: DecodeBits FlaggedByteAttribute
decodeFlaggedByteAttributeBits =
  FlaggedByteAttribute <$> getBool <*> decodeWord8leBits
