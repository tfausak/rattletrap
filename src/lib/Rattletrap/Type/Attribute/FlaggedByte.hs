{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedByte where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data FlaggedByteAttribute = FlaggedByteAttribute
  { flag :: Bool
  , byte :: Word8le.Word8le
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedByteAttribute)

bitPut :: FlaggedByteAttribute -> BitPut ()
bitPut flaggedByteAttribute = do
  BinaryBits.putBool (flag flaggedByteAttribute)
  Word8le.bitPut (byte flaggedByteAttribute)

bitGet :: BitGet FlaggedByteAttribute
bitGet =
  FlaggedByteAttribute <$> getBool <*> Word8le.bitGet
