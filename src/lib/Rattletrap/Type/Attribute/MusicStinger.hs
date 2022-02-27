module Rattletrap.Type.Attribute.MusicStinger where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

data MusicStinger = MusicStinger
  { flag :: Bool
  , cue :: U32.U32
  , trigger :: U8.U8
  }
  deriving (Eq, Show)

instance Argo.HasCodec MusicStinger where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ MusicStinger
      <$> Argo.required flag "flag"
      <*> Argo.required cue "cue"
      <*> Argo.required trigger "trigger"

bitPut :: MusicStinger -> BitPut.BitPut
bitPut musicStingerAttribute =
  BitPut.bool (flag musicStingerAttribute)
    <> U32.bitPut (cue musicStingerAttribute)
    <> U8.bitPut (trigger musicStingerAttribute)

bitGet :: BitGet.BitGet MusicStinger
bitGet = BitGet.label "MusicStinger" $ do
  flag <- BitGet.label "flag" BitGet.bool
  cue <- BitGet.label "cue" U32.bitGet
  trigger <- BitGet.label "trigger" U8.bitGet
  pure MusicStinger { flag, cue, trigger }
