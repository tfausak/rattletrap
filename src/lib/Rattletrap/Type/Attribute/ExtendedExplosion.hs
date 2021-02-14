{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ExtendedExplosion where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

data ExtendedExplosion = ExtendedExplosion
  { explosion :: Explosion.Explosion
  , unknown :: FlaggedInt.FlaggedInt
  }
  deriving (Eq, Show)

$(deriveJson ''ExtendedExplosion)

bitPut
  :: ExtendedExplosion -> BitPut.BitPut
bitPut x = do
  Explosion.bitPut (explosion x)
  FlaggedInt.bitPut (unknown x)

bitGet
  :: (Int, Int, Int) -> BitGet ExtendedExplosion
bitGet version =
  ExtendedExplosion
    <$> Explosion.bitGet version
    <*> FlaggedInt.bitGet
