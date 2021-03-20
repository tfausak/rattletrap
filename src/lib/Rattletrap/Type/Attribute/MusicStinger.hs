module Rattletrap.Type.Attribute.MusicStinger where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

data MusicStinger = MusicStinger
  { flag :: Bool
  , cue :: U32.U32
  , trigger :: U8.U8
  }
  deriving (Eq, Show)

instance Json.FromJSON MusicStinger where
  parseJSON = Json.withObject "MusicStinger" $ \object -> do
    flag <- Json.required object "flag"
    cue <- Json.required object "cue"
    trigger <- Json.required object "trigger"
    pure MusicStinger { flag, cue, trigger }

instance Json.ToJSON MusicStinger where
  toJSON x = Json.object
    [ Json.pair "flag" $ flag x
    , Json.pair "cue" $ cue x
    , Json.pair "trigger" $ trigger x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-music-stinger" $ Schema.object
  [ (Json.pair "flag" $ Schema.ref Schema.boolean, True)
  , (Json.pair "cue" $ Schema.ref U32.schema, True)
  , (Json.pair "trigger" $ Schema.ref U8.schema, True)
  ]

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
