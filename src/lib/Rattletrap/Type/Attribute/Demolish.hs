module Rattletrap.Type.Attribute.Demolish where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Demolish = Demolish
  { attackerFlag :: Bool
  , attackerActorId :: U32.U32
  , victimFlag :: Bool
  , victimActorId :: U32.U32
  , attackerVelocity :: Vector.Vector
  , victimVelocity :: Vector.Vector
  }
  deriving (Eq, Show)

instance Json.FromJSON Demolish where
  parseJSON = Json.withObject "Demolish" $ \object -> do
    attackerFlag <- Json.required object "attacker_flag"
    attackerActorId <- Json.required object "attacker_actor_id"
    victimFlag <- Json.required object "victim_flag"
    victimActorId <- Json.required object "victim_actor_id"
    attackerVelocity <- Json.required object "attacker_velocity"
    victimVelocity <- Json.required object "victim_velocity"
    pure Demolish
      { attackerFlag
      , attackerActorId
      , victimFlag
      , victimActorId
      , attackerVelocity
      , victimVelocity
      }

instance Json.ToJSON Demolish where
  toJSON x = Json.object
    [ Json.pair "attacker_flag" $ attackerFlag x
    , Json.pair "attacker_actor_id" $ attackerActorId x
    , Json.pair "victim_flag" $ victimFlag x
    , Json.pair "victim_actor_id" $ victimActorId x
    , Json.pair "attacker_velocity" $ attackerVelocity x
    , Json.pair "victim_velocity" $ victimVelocity x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-demolish" $ Schema.object
  [ (Json.pair "attacker_flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "attacker_actor_id" $ Schema.ref U32.schema, True)
  , (Json.pair "victim_flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "victim_actor_id" $ Schema.ref U32.schema, True)
  , (Json.pair "attacker_velocity" $ Schema.ref Vector.schema, True)
  , (Json.pair "victim_velocity" $ Schema.ref Vector.schema, True)
  ]

bitPut :: Demolish -> BitPut.BitPut
bitPut demolishAttribute =
  BitPut.bool (attackerFlag demolishAttribute)
    <> U32.bitPut (attackerActorId demolishAttribute)
    <> BitPut.bool (victimFlag demolishAttribute)
    <> U32.bitPut (victimActorId demolishAttribute)
    <> Vector.bitPut (attackerVelocity demolishAttribute)
    <> Vector.bitPut (victimVelocity demolishAttribute)

bitGet :: Version.Version -> BitGet.BitGet Demolish
bitGet version = do
  attackerFlag <- BitGet.bool
  attackerActorId <- U32.bitGet
  victimFlag <- BitGet.bool
  victimActorId <- U32.bitGet
  attackerVelocity <- Vector.bitGet version
  victimVelocity <- Vector.bitGet version
  pure Demolish
    { attackerFlag
    , attackerActorId
    , victimFlag
    , victimActorId
    , attackerVelocity
    , victimVelocity
    }
