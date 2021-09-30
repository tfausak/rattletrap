module Rattletrap.Type.RemoteId.PsyNet where

import qualified Control.Applicative as Applicative
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data PsyNet
  = New U64.U64
  | Old U64.U64 U64.U64 U64.U64 U64.U64
  deriving (Eq, Show)

instance Json.FromJSON PsyNet where
  parseJSON = Json.withObject "PsyNet" $ \object -> do
    let
      new = fmap New $ Json.required object "Left"
      old = do
        [a, b, c, d] <- Json.required object "Right"
        pure $ Old a b c d
    new Applicative.<|> old

instance Json.ToJSON PsyNet where
  toJSON x = case x of
    New a -> Json.object [Json.pair "Left" a]
    Old a b c d -> Json.object [Json.pair "Right" [a, b, c, d]]

schema :: Schema.Schema
schema = Schema.named "remote-id-psy-net" $ Schema.oneOf
  [ Schema.object [(Json.pair "Left" $ Schema.ref U64.schema, True)]
  , Schema.object
    [ ( Json.pair "Right" . Schema.tuple . replicate 4 $ Schema.ref U64.schema
      , True
      )
    ]
  ]

bitPut :: PsyNet -> BitPut.BitPut
bitPut x = case x of
  New l -> U64.bitPut l
  Old a b c d -> U64.bitPut a <> U64.bitPut b <> U64.bitPut c <> U64.bitPut d

bitGet :: Version.Version -> BitGet.BitGet PsyNet
bitGet version = BitGet.label "PsyNet" $ if Version.atLeast 868 24 10 version
  then BitGet.label "New" $ fmap New U64.bitGet
  else BitGet.label "Old" $ do
    a <- U64.bitGet
    b <- U64.bitGet
    c <- U64.bitGet
    d <- U64.bitGet
    pure $ Old a b c d
