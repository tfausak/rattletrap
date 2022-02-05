module Rattletrap.Type.Attribute.StatEvent where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Vendor.Argo as Argo

data StatEvent = StatEvent
  { unknown :: Bool
  , objectId :: I32.I32
  }
  deriving (Eq, Show)

instance Argo.HasCodec StatEvent where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ StatEvent
      <$> Argo.project
            unknown
            (Argo.required (Argo.fromString "unknown") Argo.codec)
      <*> Argo.project
            objectId
            (Argo.required (Argo.fromString "object_id") Argo.codec)

bitPut :: StatEvent -> BitPut.BitPut
bitPut statEventAttribute = BitPut.bool (unknown statEventAttribute)
  <> I32.bitPut (objectId statEventAttribute)

bitGet :: BitGet.BitGet StatEvent
bitGet = BitGet.label "StatEvent" $ do
  unknown <- BitGet.label "unknown" BitGet.bool
  objectId <- BitGet.label "objectId" I32.bitGet
  pure StatEvent { unknown, objectId }
