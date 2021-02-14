{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ExtendedExplosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { explosion :: Explosion.ExplosionAttribute
  , unknown :: FlaggedInt.FlaggedIntAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''ExtendedExplosionAttribute)

bitPut
  :: ExtendedExplosionAttribute -> BitPut ()
bitPut x = do
  Explosion.bitPut (explosion x)
  FlaggedInt.bitPut (unknown x)

bitGet
  :: (Int, Int, Int) -> BitGet ExtendedExplosionAttribute
bitGet version =
  ExtendedExplosionAttribute
    <$> Explosion.bitGet version
    <*> FlaggedInt.bitGet
