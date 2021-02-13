{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedByte where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedByteAttribute = FlaggedByteAttribute
  { flaggedByteAttributeFlag :: Bool
  , flaggedByteAttributeByte :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedByteAttribute)

putFlaggedByteAttribute :: FlaggedByteAttribute -> BitPut ()
putFlaggedByteAttribute flaggedByteAttribute = do
  BinaryBits.putBool (flaggedByteAttributeFlag flaggedByteAttribute)
  Word8le.bitPut (flaggedByteAttributeByte flaggedByteAttribute)

decodeFlaggedByteAttributeBits :: BitGet FlaggedByteAttribute
decodeFlaggedByteAttributeBits =
  FlaggedByteAttribute <$> getBool <*> Word8le.bitGet
