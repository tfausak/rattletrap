module Rattletrap.Type.Attribute.CustomDemolishExtended where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.CustomDemolish as CustomDemolish
import qualified Rattletrap.Type.ObjectTarget as ObjectTarget
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data CustomDemolishExtended = CustomDemolishExtended
  { attackerPri :: ObjectTarget.ObjectTarget,
    selfDemoFx :: ObjectTarget.ObjectTarget,
    selfDemolish :: Bool,
    customDemolish :: CustomDemolish.CustomDemolish
  }
  deriving (Eq, Show)

instance Json.FromJSON CustomDemolishExtended where
  parseJSON = Json.withObject "CustomDemolishExtended" $ \object -> do
    attackerPri <- Json.required object "attacker_pri"
    selfDemoFx <- Json.required object "self_demo_fx"
    selfDemolish <- Json.required object "self_demolish"
    customDemolish <- Json.required object "custom_demolish"
    pure
      CustomDemolishExtended
        { attackerPri,
          selfDemoFx,
          selfDemolish,
          customDemolish
        }

instance Json.ToJSON CustomDemolishExtended where
  toJSON x =
    Json.object
      [ Json.pair "attacker_pri" $ attackerPri x,
        Json.pair "self_demo_fx" $ selfDemoFx x,
        Json.pair "self_demolish" $ selfDemolish x,
        Json.pair "custom_demolish" $ customDemolish x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute-demolish" $
    Schema.object
      [ (Json.pair "attacker_pri" $ Schema.ref ObjectTarget.schema, True),
        (Json.pair "self_demo_fx" $ Schema.ref ObjectTarget.schema, True),
        (Json.pair "self_demolish" $ Schema.ref Schema.boolean, True),
        (Json.pair "custom_demolish" $ Schema.ref CustomDemolish.schema, True)
      ]

bitPut :: CustomDemolishExtended -> BitPut.BitPut
bitPut demolishAttribute =
  ObjectTarget.bitPut (attackerPri demolishAttribute)
    <> ObjectTarget.bitPut (selfDemoFx demolishAttribute)
    <> BitPut.bool (selfDemolish demolishAttribute)
    <> CustomDemolish.bitPut (customDemolish demolishAttribute)

bitGet :: Version.Version -> BitGet.BitGet CustomDemolishExtended
bitGet version = BitGet.label "CustomDemolishExtended" $ do
  attackerPri <- BitGet.label "attackerPri" ObjectTarget.bitGet
  selfDemoFx <- BitGet.label "selfDemoFx" ObjectTarget.bitGet
  selfDemolish <- BitGet.label "selfDemolish" BitGet.bool
  customDemolish <- BitGet.label "customDemolish" $ CustomDemolish.bitGet version
  pure
    CustomDemolishExtended
      { attackerPri,
        selfDemoFx,
        selfDemolish,
        customDemolish
      }
