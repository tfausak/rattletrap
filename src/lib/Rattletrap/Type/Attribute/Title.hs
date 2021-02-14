{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Title where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Title = Title
  { unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: U32.U32
  , unknown4 :: U32.U32
  , unknown5 :: U32.U32
  , unknown6 :: U32.U32
  , unknown7 :: U32.U32
  , unknown8 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''Title)

bitPut :: Title -> BitPut ()
bitPut titleAttribute = do
  BinaryBits.putBool (unknown1 titleAttribute)
  BinaryBits.putBool (unknown2 titleAttribute)
  U32.bitPut (unknown3 titleAttribute)
  U32.bitPut (unknown4 titleAttribute)
  U32.bitPut (unknown5 titleAttribute)
  U32.bitPut (unknown6 titleAttribute)
  U32.bitPut (unknown7 titleAttribute)
  BinaryBits.putBool (unknown8 titleAttribute)

bitGet :: BitGet Title
bitGet =
  Title
    <$> getBool
    <*> getBool
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> U32.bitGet
    <*> getBool
