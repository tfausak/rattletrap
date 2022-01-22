module Rattletrap.Type.Attribute.ExtendedExplosion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data ExtendedExplosion = ExtendedExplosion
  { explosion :: Explosion.Explosion
  , unknown :: FlaggedInt.FlaggedInt
  }
  deriving (Eq, Show)

instance Argo.HasCodec ExtendedExplosion where
  codec =
    Argo.fromObjectCodec Argo.Allow
      $ ExtendedExplosion
      <$> Argo.project
            explosion
            (Argo.required (Argo.fromString "explosion") Argo.codec)
      <*> Argo.project
            unknown
            (Argo.required (Argo.fromString "unknown") Argo.codec)

bitPut :: ExtendedExplosion -> BitPut.BitPut
bitPut x = Explosion.bitPut (explosion x) <> FlaggedInt.bitPut (unknown x)

bitGet :: Version.Version -> BitGet.BitGet ExtendedExplosion
bitGet version = BitGet.label "ExtendedExplosion" $ do
  explosion <- BitGet.label "explosion" $ Explosion.bitGet version
  unknown <- BitGet.label "unknown" FlaggedInt.bitGet
  pure ExtendedExplosion { explosion, unknown }
