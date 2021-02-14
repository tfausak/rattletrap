{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Explosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

data Explosion = Explosion
  { flag :: Bool
  , actorId :: I32.I32
  , location :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''Explosion)

bitPut :: Explosion -> BitPut.BitPut
bitPut explosionAttribute =
  BitPut.bool (flag explosionAttribute)
  <> I32.bitPut (actorId explosionAttribute)
  <> Vector.bitPut (location explosionAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet.BitGet Explosion
bitGet version =
  Explosion
    <$> BitGet.bool
    <*> I32.bitGet
    <*> Vector.bitGet version
