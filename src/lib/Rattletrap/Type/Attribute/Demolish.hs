module Rattletrap.Type.Attribute.Demolish where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Demolish = Demolish
  { attackerFlag :: Bool
  , attackerActorId :: U32.U32
  , victimFlag :: Bool
  , victimActorId :: U32.U32
  , attackerVelocity :: Vector.Vector
  , victimVelocity :: Vector.Vector
  }
  deriving (Eq, Show)

instance Argo.HasCodec Demolish where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Demolish
      <$> Argo.required attackerFlag "attacker_flag"
      <*> Argo.required attackerActorId "attacker_actor_id"
      <*> Argo.required victimFlag "victim_flag"
      <*> Argo.required victimActorId "victim_actor_id"
      <*> Argo.required attackerVelocity "attacker_velocity"
      <*> Argo.required victimVelocity "victim_velocity"

bitPut :: Demolish -> BitPut.BitPut
bitPut demolishAttribute =
  BitPut.bool (attackerFlag demolishAttribute)
    <> U32.bitPut (attackerActorId demolishAttribute)
    <> BitPut.bool (victimFlag demolishAttribute)
    <> U32.bitPut (victimActorId demolishAttribute)
    <> Vector.bitPut (attackerVelocity demolishAttribute)
    <> Vector.bitPut (victimVelocity demolishAttribute)

bitGet :: Version.Version -> BitGet.BitGet Demolish
bitGet version = BitGet.label "Demolish" $ do
  attackerFlag <- BitGet.label "attackerFlag" BitGet.bool
  attackerActorId <- BitGet.label "attackerActorId" U32.bitGet
  victimFlag <- BitGet.label "victimFlag" BitGet.bool
  victimActorId <- BitGet.label "victimActorId" U32.bitGet
  attackerVelocity <- BitGet.label "attackerVelocity" $ Vector.bitGet version
  victimVelocity <- BitGet.label "victimVelocity" $ Vector.bitGet version
  pure Demolish
    { attackerFlag
    , attackerActorId
    , victimFlag
    , victimActorId
    , attackerVelocity
    , victimVelocity
    }
