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
    Argo.fromObjectCodec Argo.Allow
      $ Demolish
      <$> Argo.project
            attackerFlag
            (Argo.required (Argo.fromString "attacker_flag") Argo.codec)
      <*> Argo.project
            attackerActorId
            (Argo.required (Argo.fromString "attacker_actor_id") Argo.codec)
      <*> Argo.project
            victimFlag
            (Argo.required (Argo.fromString "victim_flag") Argo.codec)
      <*> Argo.project
            victimActorId
            (Argo.required (Argo.fromString "victim_actor_id") Argo.codec)
      <*> Argo.project
            attackerVelocity
            (Argo.required (Argo.fromString "attacker_velocity") Argo.codec)
      <*> Argo.project
            victimVelocity
            (Argo.required (Argo.fromString "victim_velocity") Argo.codec)

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
