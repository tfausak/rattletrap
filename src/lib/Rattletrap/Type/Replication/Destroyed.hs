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

instance Json.FromValue Destroyed where
  fromValue json = do
    () <- Json.fromValue json
    pure Destroyed

instance Json.ToValue Destroyed where
  toValue = const $ Json.toValue ()

schema :: Schema.Schema
schema =
  Schema.named "replication-destroyed" $ Json.object [Json.pair "type" "array"]

bitPut :: Destroyed -> BitPut.BitPut
bitPut _ = mempty

bitGet :: BitGet.BitGet Destroyed
bitGet = BitGet.label "Destroyed" $ pure Destroyed
