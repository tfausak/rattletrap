module Rattletrap.Type.Attribute.ExtendedExplosion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data ExtendedExplosion = ExtendedExplosion
  { explosion :: Explosion.Explosion
  , unknown :: FlaggedInt.FlaggedInt
  }
  deriving (Eq, Show)

$(deriveJson ''ExtendedExplosion)

schema :: Schema.Schema
schema = Schema.named "attribute-extended-explosion" $ Schema.object
  [ (Json.pair "explosion" $ Schema.ref Explosion.schema, True)
  , (Json.pair "unknown" $ Schema.ref FlaggedInt.schema, True)
  ]

bitPut :: ExtendedExplosion -> BitPut.BitPut
bitPut x = Explosion.bitPut (explosion x) <> FlaggedInt.bitPut (unknown x)

bitGet :: Version.Version -> BitGet.BitGet ExtendedExplosion
bitGet version =
  ExtendedExplosion <$> Explosion.bitGet version <*> FlaggedInt.bitGet
