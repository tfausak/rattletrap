{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.DamageState where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

data DamageState = DamageState
  { unknown1 :: U8.U8
  , unknown2 :: Bool
  , unknown3 :: I32.I32
  , unknown4 :: Vector.Vector
  , unknown5 :: Bool
  , unknown6 :: Bool
  }
  deriving (Eq, Show)

$(deriveJson ''DamageState)

bitPut :: DamageState -> BitPut.BitPut
bitPut damageStateAttribute = do
  U8.bitPut (unknown1 damageStateAttribute)
  BitPut.bool (unknown2 damageStateAttribute)
  I32.bitPut (unknown3 damageStateAttribute)
  Vector.bitPut (unknown4 damageStateAttribute)
  BitPut.bool (unknown5 damageStateAttribute)
  BitPut.bool (unknown6 damageStateAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet DamageState
bitGet version =
  DamageState
    <$> U8.bitGet
    <*> getBool
    <*> I32.bitGet
    <*> Vector.bitGet version
    <*> getBool
    <*> getBool
