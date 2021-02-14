{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ExtendedExplosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

data ExtendedExplosion = ExtendedExplosion
  { explosion :: Explosion.Explosion
  , unknown :: FlaggedInt.FlaggedInt
  }
  deriving (Eq, Show)

$(deriveJson ''ExtendedExplosion)

bitPut
  :: ExtendedExplosion -> BitPut.BitPut
bitPut x =
  Explosion.bitPut (explosion x)
  <> FlaggedInt.bitPut (unknown x)

bitGet
  :: (Int, Int, Int) -> BitGet.BitGet ExtendedExplosion
bitGet version =
  ExtendedExplosion
    <$> Explosion.bitGet version
    <*> FlaggedInt.bitGet
