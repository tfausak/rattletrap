module Rattletrap.Type.Attribute.Pickup where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

data Pickup = Pickup
  { instigatorId :: Maybe U32.U32
  , pickedUp :: Bool
  }
  deriving (Eq, Show)

instance Json.FromValue Pickup where
  fromValue = Json.withObject "Pickup" $ \object -> do
    instigatorId <- Json.optional object "instigator_id"
    pickedUp <- Json.required object "picked_up"
    pure Pickup { instigatorId, pickedUp }

instance Json.ToValue Pickup where
  toValue x = Json.object
    [ Json.pair "instigator_id" $ instigatorId x
    , Json.pair "picked_up" $ pickedUp x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-pickup" $ Schema.object
  [ (Json.pair "instigator_id" . Schema.json $ Schema.maybe U32.schema, False)
  , (Json.pair "picked_up" $ Schema.ref Schema.boolean, True)
  ]

bitPut :: Pickup -> BitPut.BitPut
bitPut x =
  maybe
      (BitPut.bool False)
      (\y -> BitPut.bool True <> U32.bitPut y)
      (instigatorId x)
    <> BitPut.bool (pickedUp x)

bitGet :: BitGet.BitGet Pickup
bitGet = BitGet.label "Pickup" $ do
  instigator <- BitGet.label "instigator" BitGet.bool
  instigatorId <- BitGet.label "instigatorId"
    $ Monad.whenMaybe instigator U32.bitGet
  pickedUp <- BitGet.label "pickedUp" BitGet.bool
  pure Pickup { instigatorId, pickedUp }
