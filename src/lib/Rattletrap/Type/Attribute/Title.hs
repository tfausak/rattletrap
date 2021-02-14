{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Title where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data TitleAttribute = TitleAttribute
  { unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: Word32le.Word32le
  , unknown4 :: Word32le.Word32le
  , unknown5 :: Word32le.Word32le
  , unknown6 :: Word32le.Word32le
  , unknown7 :: Word32le.Word32le
  , unknown8 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''TitleAttribute)

bitPut :: TitleAttribute -> BitPut ()
bitPut titleAttribute = do
  BinaryBits.putBool (unknown1 titleAttribute)
  BinaryBits.putBool (unknown2 titleAttribute)
  Word32le.bitPut (unknown3 titleAttribute)
  Word32le.bitPut (unknown4 titleAttribute)
  Word32le.bitPut (unknown5 titleAttribute)
  Word32le.bitPut (unknown6 titleAttribute)
  Word32le.bitPut (unknown7 titleAttribute)
  BinaryBits.putBool (unknown8 titleAttribute)

bitGet :: BitGet TitleAttribute
bitGet =
  TitleAttribute
    <$> getBool
    <*> getBool
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> getBool
