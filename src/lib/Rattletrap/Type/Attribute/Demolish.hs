module Rattletrap.Type.Attribute.Demolish where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version

data Demolish = Demolish
  { attackerFlag :: Bool
  , attackerActorId :: U32.U32
  , victimFlag :: Bool
  , victimActorId :: U32.U32
  , attackerVelocity :: Vector.Vector
  , victimVelocity :: Vector.Vector
  }
  deriving (Eq, Show)

$(deriveJson ''Demolish)

bitPut :: Demolish -> BitPut.BitPut
bitPut demolishAttribute =
  BitPut.bool (attackerFlag demolishAttribute)
    <> U32.bitPut (attackerActorId demolishAttribute)
    <> BitPut.bool (victimFlag demolishAttribute)
    <> U32.bitPut (victimActorId demolishAttribute)
    <> Vector.bitPut (attackerVelocity demolishAttribute)
    <> Vector.bitPut (victimVelocity demolishAttribute)

bitGet :: Version.Version -> BitGet.BitGet Demolish
bitGet version =
  Demolish
    <$> BitGet.bool
    <*> U32.bitGet
    <*> BitGet.bool
    <*> U32.bitGet
    <*> Vector.bitGet version
    <*> Vector.bitGet version
