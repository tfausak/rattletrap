{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Explosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Explosion = Explosion
  { flag :: Bool
  , actorId :: I32.I32
  , location :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''Explosion)

bitPut :: Explosion -> BitPut ()
bitPut explosionAttribute = do
  BinaryBits.putBool (flag explosionAttribute)
  I32.bitPut (actorId explosionAttribute)
  Vector.bitPut (location explosionAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet Explosion
bitGet version =
  Explosion
    <$> getBool
    <*> I32.bitGet
    <*> Vector.bitGet version
