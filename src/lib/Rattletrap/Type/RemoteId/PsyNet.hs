module Rattletrap.Type.RemoteId.PsyNet where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

newtype PsyNet
  = PsyNet (Either U64.U64 (U64.U64, U64.U64, U64.U64, U64.U64))
  deriving (Eq, Show)

instance Json.FromJSON PsyNet where
  parseJSON = fmap fromEither . Json.parseJSON

instance Json.ToJSON PsyNet where
  toJSON = Json.toJSON . toEither

fromEither :: Either U64.U64 (U64.U64, U64.U64, U64.U64, U64.U64) -> PsyNet
fromEither = PsyNet

toEither :: PsyNet -> Either U64.U64 (U64.U64, U64.U64, U64.U64, U64.U64)
toEither (PsyNet x) = x

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
bitPut x = case toEither x of
  Left l -> U64.bitPut l
  Right (a, b, c, d) ->
    U64.bitPut a <> U64.bitPut b <> U64.bitPut c <> U64.bitPut d

bitGet :: Version.Version -> BitGet.BitGet PsyNet
bitGet version =
  BitGet.label "PsyNet"
    . fmap fromEither
    $ if Version.atLeast 868 24 10 version
        then BitGet.label "new" $ fmap Left U64.bitGet
        else BitGet.label "old" . fmap Right $ do
          a <- U64.bitGet
          b <- U64.bitGet
          c <- U64.bitGet
          d <- U64.bitGet
          pure (a, b, c, d)
