{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Title where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data TitleAttribute = TitleAttribute
  { titleAttributeUnknown1 :: Bool
  , titleAttributeUnknown2 :: Bool
  , titleAttributeUnknown3 :: Word32le
  , titleAttributeUnknown4 :: Word32le
  , titleAttributeUnknown5 :: Word32le
  , titleAttributeUnknown6 :: Word32le
  , titleAttributeUnknown7 :: Word32le
  , titleAttributeUnknown8 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''TitleAttribute)

putTitleAttribute :: TitleAttribute -> BinaryBits.BitPut ()
putTitleAttribute titleAttribute = do
  BinaryBits.putBool (titleAttributeUnknown1 titleAttribute)
  BinaryBits.putBool (titleAttributeUnknown2 titleAttribute)
  putWord32Bits (titleAttributeUnknown3 titleAttribute)
  putWord32Bits (titleAttributeUnknown4 titleAttribute)
  putWord32Bits (titleAttributeUnknown5 titleAttribute)
  putWord32Bits (titleAttributeUnknown6 titleAttribute)
  putWord32Bits (titleAttributeUnknown7 titleAttribute)
  BinaryBits.putBool (titleAttributeUnknown8 titleAttribute)

decodeTitleAttributeBits :: DecodeBits TitleAttribute
decodeTitleAttributeBits =
  TitleAttribute
    <$> getBool
    <*> getBool
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> decodeWord32leBits
    <*> getBool
