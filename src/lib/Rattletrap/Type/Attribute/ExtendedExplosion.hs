module Rattletrap.Type.Attribute.ExtendedExplosion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data ExtendedExplosion = ExtendedExplosion
  { explosion :: Explosion.Explosion
  , unknown :: FlaggedInt.FlaggedInt
  }
  deriving (Eq, Show)

instance Json.FromJSON ExtendedExplosion where
  parseJSON = Json.withObject "ExtendedExplosion" $ \object -> do
    explosion <- Json.required object "explosion"
    unknown <- Json.required object "unknown"
    pure ExtendedExplosion { explosion, unknown }

instance Json.ToJSON ExtendedExplosion where
  toJSON x = Json.object
    [Json.pair "explosion" $ explosion x, Json.pair "unknown" $ unknown x]

schema :: Schema.Schema
schema = Schema.named "attribute-extended-explosion" $ Schema.object
  [ (Json.pair "explosion" $ Schema.ref Explosion.schema, True)
  , (Json.pair "unknown" $ Schema.ref FlaggedInt.schema, True)
  ]

bitPut :: ExtendedExplosion -> BitPut.BitPut
bitPut x = Explosion.bitPut (explosion x) <> FlaggedInt.bitPut (unknown x)

bitGet :: Version.Version -> BitGet.BitGet ExtendedExplosion
bitGet version = do
  explosion <- Explosion.bitGet version
  unknown <- FlaggedInt.bitGet
  pure ExtendedExplosion { explosion, unknown }
