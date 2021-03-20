module Rattletrap.Type.Attribute.PickupNew where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data PickupNew = PickupNew
  { instigatorId :: Maybe U32.U32
  , pickedUp :: U8.U8
  }
  deriving (Eq, Show)

instance Json.FromJSON PickupNew where
  parseJSON = Json.withObject "PickupNew" $ \object -> do
    instigatorId <- Json.optional object "instigator_id"
    pickedUp <- Json.required object "picked_up"
    pure PickupNew { instigatorId, pickedUp }

instance Json.ToJSON PickupNew where
  toJSON x = Json.object
    [ Json.pair "instigator_id" $ instigatorId x
    , Json.pair "picked_up" $ pickedUp x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-pickup-new" $ Schema.object
  [ (Json.pair "instigator_id" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "picked_up" $ Schema.ref U8.schema, True)
  ]

bitPut :: PickupNew -> BitPut.BitPut
bitPut x =
  maybe
      (BitPut.bool False)
      (\y -> BitPut.bool True <> U32.bitPut y)
      (instigatorId x)
    <> U8.bitPut (pickedUp x)

bitGet :: BitGet.BitGet PickupNew
bitGet = BitGet.label "PickupNew" $ do
  instigator <- BitGet.label "instigator" BitGet.bool
  instigatorId <- BitGet.label "instigatorId"
    $ Monad.whenMaybe instigator U32.bitGet
  pickedUp <- BitGet.label "pickedUp" U8.bitGet
  pure PickupNew { instigatorId, pickedUp }
