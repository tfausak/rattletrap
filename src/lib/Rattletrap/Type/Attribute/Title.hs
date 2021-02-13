{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Title where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data TitleAttribute = TitleAttribute
  { titleAttributeUnknown1 :: Bool
  , titleAttributeUnknown2 :: Bool
  , titleAttributeUnknown3 :: Word32le.Word32le
  , titleAttributeUnknown4 :: Word32le.Word32le
  , titleAttributeUnknown5 :: Word32le.Word32le
  , titleAttributeUnknown6 :: Word32le.Word32le
  , titleAttributeUnknown7 :: Word32le.Word32le
  , titleAttributeUnknown8 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''TitleAttribute)

putTitleAttribute :: TitleAttribute -> BitPut ()
putTitleAttribute titleAttribute = do
  BinaryBits.putBool (titleAttributeUnknown1 titleAttribute)
  BinaryBits.putBool (titleAttributeUnknown2 titleAttribute)
  Word32le.bitPut (titleAttributeUnknown3 titleAttribute)
  Word32le.bitPut (titleAttributeUnknown4 titleAttribute)
  Word32le.bitPut (titleAttributeUnknown5 titleAttribute)
  Word32le.bitPut (titleAttributeUnknown6 titleAttribute)
  Word32le.bitPut (titleAttributeUnknown7 titleAttribute)
  BinaryBits.putBool (titleAttributeUnknown8 titleAttribute)

decodeTitleAttributeBits :: BitGet TitleAttribute
decodeTitleAttributeBits =
  TitleAttribute
    <$> getBool
    <*> getBool
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> Word32le.bitGet
    <*> getBool
