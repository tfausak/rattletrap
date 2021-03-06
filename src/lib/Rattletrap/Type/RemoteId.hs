module Rattletrap.Type.RemoteId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.RemoteId.Epic as Epic
import qualified Rattletrap.Type.RemoteId.PlayStation as PlayStation
import qualified Rattletrap.Type.RemoteId.Xbox as Xbox
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

import qualified Data.Foldable as Foldable
import qualified Data.Word as Word

data RemoteId
  = PlayStation PlayStation.PlayStation
  | PsyNet (Either U64.U64 (U64.U64, U64.U64, U64.U64, U64.U64))
  | Splitscreen Word.Word32
  -- ^ Really only 24 bits.
  | Steam U64.U64
  | Switch U64.U64 U64.U64 U64.U64 U64.U64
  | Xbox Xbox.Xbox
  | Epic Epic.Epic
  deriving (Eq, Show)

instance Json.FromJSON RemoteId where
  parseJSON = Json.withObject "RemoteId" $ \object -> Foldable.asum
    [ fmap PlayStation $ Json.required object "play_station"
    , fmap PsyNet $ Json.required object "psy_net"
    , fmap Splitscreen $ Json.required object "splitscreen"
    , fmap Steam $ Json.required object "steam"
    , fmap (uncurry4 Switch) $ Json.required object "switch"
    , fmap Xbox $ Json.required object "xbox"
    , fmap Epic $ Json.required object "epic"
    ]

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

instance Json.ToJSON RemoteId where
  toJSON x = case x of
    PlayStation y -> Json.object [Json.pair "play_station" y]
    PsyNet y -> Json.object [Json.pair "psy_net" y]
    Splitscreen y -> Json.object [Json.pair "splitscreen" y]
    Steam y -> Json.object [Json.pair "steam" y]
    Switch y z a b -> Json.object [Json.pair "switch" (y, z, a, b)]
    Xbox y -> Json.object [Json.pair "xbox" y]
    Epic y -> Json.object [Json.pair "epic" y]

schema :: Schema.Schema
schema = Schema.named "remote-id" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k v, True)])
  [ ("play_station", Schema.ref PlayStation.schema)
  , ( "psy_net"
    , Schema.oneOf
      [ Schema.object [(Json.pair "Left" $ Schema.ref U64.schema, True)]
      , Schema.object
        [ ( Json.pair "Right" . Schema.tuple . replicate 4 $ Schema.ref
            U64.schema
          , True
          )
        ]
      ]
    )
  , ("splitscreen", Schema.ref Schema.integer)
  , ("steam", Schema.ref U64.schema)
  , ("switch", Schema.tuple . replicate 4 $ Schema.ref U64.schema)
  , ("xbox", Schema.ref Xbox.schema)
  , ("epic", Schema.ref Epic.schema)
  ]

bitPut :: RemoteId -> BitPut.BitPut
bitPut remoteId = case remoteId of
  PlayStation x -> PlayStation.bitPut x
  PsyNet e -> case e of
    Left l -> U64.bitPut l
    Right (a, b, c, d) -> putWord256 a b c d
  Splitscreen word24 -> BitPut.bits 24 word24
  Steam word64 -> U64.bitPut word64
  Switch a b c d -> putWord256 a b c d
  Xbox x -> Xbox.bitPut x
  Epic x -> Epic.bitPut x

putWord256 :: U64.U64 -> U64.U64 -> U64.U64 -> U64.U64 -> BitPut.BitPut
putWord256 a b c d =
  U64.bitPut a <> U64.bitPut b <> U64.bitPut c <> U64.bitPut d

bitGet :: Version.Version -> U8.U8 -> BitGet.BitGet RemoteId
bitGet version systemId = case U8.toWord8 systemId of
  0 -> fmap Splitscreen $ BitGet.bits 24
  1 -> fmap Steam U64.bitGet
  2 -> fmap PlayStation $ PlayStation.bitGet version
  4 -> fmap Xbox Xbox.bitGet
  6 -> do
    (a, b, c, d) <- getWord256
    pure $ Switch a b c d
  7 -> fmap PsyNet $ if Version.atLeast 868 24 10 version
    then fmap Left U64.bitGet
    else fmap Right getWord256
  11 -> fmap Epic Epic.bitGet
  _ -> fail ("[RT09] unknown system id " <> show systemId)

getWord256 :: BitGet.BitGet (U64.U64, U64.U64, U64.U64, U64.U64)
getWord256 = do
  a <- U64.bitGet
  b <- U64.bitGet
  c <- U64.bitGet
  d <- U64.bitGet
  pure (a, b, c, d)
