module Rattletrap.Type.Replication.Destroyed where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Utility.Json as Json

-- | Destroyed replications don't actually contain any extra information. All
-- you need to know is the actor's ID, which is given by the
-- 'Rattletrap.Replication.Replication'.
data Destroyed = Destroyed
  deriving (Eq, Show)

instance Json.FromJSON Destroyed where
  parseJSON json = do
    () <- Json.parseJSON json
    pure Destroyed

instance Json.ToJSON Destroyed where
  toJSON = const $ Json.toJSON ()

schema :: Schema.Schema
schema = Schema.named "replication-destroyed"
  $ Json.object [Json.pair "type" "array"]

bitPut :: Destroyed -> BitPut.BitPut
bitPut _ = mempty

bitGet :: BitGet.BitGet Destroyed
bitGet = pure Destroyed
