module Rattletrap.Type.Attribute.PickupNew where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Utility.Monad
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data PickupNew = PickupNew
  { instigatorId :: Maybe U32.U32
  , pickedUp :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''PickupNew)

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
bitGet = do
  instigator <- BitGet.bool
  PickupNew <$> whenMaybe instigator U32.bitGet <*> U8.bitGet
