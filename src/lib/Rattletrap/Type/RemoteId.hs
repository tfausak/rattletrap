module Rattletrap.Type.RemoteId where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import Rattletrap.Utility.Bytes
import qualified Rattletrap.Utility.Json as Json

import qualified Data.ByteString as Bytes
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

data RemoteId
  = PlayStation Text.Text [Word.Word8]
  | PsyNet (Either U64.U64 (U64.U64, U64.U64, U64.U64, U64.U64))
  | Splitscreen Word.Word32
  -- ^ Really only 24 bits.
  | Steam U64.U64
  | Switch U64.U64 U64.U64 U64.U64 U64.U64
  | Xbox U64.U64
  | Epic Str.Str
  deriving (Eq, Show)

instance Json.FromJSON RemoteId where
  parseJSON = Json.withObject "RemoteId" $ \object -> Foldable.asum
    [ uncurry PlayStation <$> Json.required object "play_stations"
    , PsyNet <$> Json.required object "psy_net"
    , Splitscreen <$> Json.required object "splitscreen"
    , Steam <$> Json.required object "steam"
    , uncurry4 Switch <$> Json.required object "switch"
    , Xbox <$> Json.required object "xbox"
    , Epic <$> Json.required object "epic"
    ]

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

instance Json.ToJSON RemoteId where
  toJSON x = case x of
    PlayStation y z -> Json.object [Json.pair "play_stations" (y, z)]
    PsyNet y -> Json.object [Json.pair "psy_net" y]
    Splitscreen y -> Json.object [Json.pair "splitscreen" y]
    Steam y -> Json.object [Json.pair "steam" y]
    Switch y z a b -> Json.object [Json.pair "switch" (y, z, a, b)]
    Xbox y -> Json.object [Json.pair "xbox" y]
    Epic y -> Json.object [Json.pair "epic" y]

schema :: Schema.Schema
schema = Schema.named "remote-id" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k v, True)])
  [ ( "play_station"
    , Schema.tuple
      [Schema.ref Schema.string, Schema.json $ Schema.array Schema.number]
    )
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
  , ("xbox", Schema.ref U64.schema)
  , ("epic", Schema.ref Str.schema)
  ]

bitPut :: RemoteId -> BitPut.BitPut
bitPut remoteId = case remoteId of
  PlayStation name bytes ->
    let rawName = reverseBytes (padBytes (16 :: Int) (encodeLatin1 name))
    in BitPut.byteString rawName <> BitPut.byteString (Bytes.pack bytes)
  PsyNet e -> case e of
    Left l -> U64.bitPut l
    Right (a, b, c, d) -> putWord256 a b c d
  Splitscreen word24 -> BitPut.bits 24 word24
  Steam word64 -> U64.bitPut word64
  Switch a b c d -> putWord256 a b c d
  Xbox word64 -> U64.bitPut word64
  Epic str -> Str.bitPut str

putWord256 :: U64.U64 -> U64.U64 -> U64.U64 -> U64.U64 -> BitPut.BitPut
putWord256 a b c d =
  U64.bitPut a <> U64.bitPut b <> U64.bitPut c <> U64.bitPut d

bitGet :: Version.Version -> U8.U8 -> BitGet.BitGet RemoteId
bitGet version systemId = case U8.toWord8 systemId of
  0 -> Splitscreen <$> BitGet.bits 24
  1 -> Steam <$> U64.bitGet
  2 -> PlayStation <$> decodePsName <*> decodePsBytes version
  4 -> Xbox <$> U64.bitGet
  6 -> do
    (a, b, c, d) <- getWord256
    pure $ Switch a b c d
  7 -> if psyNetIsU64 version
    then PsyNet . Left <$> U64.bitGet
    else PsyNet . Right <$> getWord256
  11 -> Epic <$> Str.bitGet
  _ -> fail ("[RT09] unknown system id " <> show systemId)

psyNetIsU64 :: Version.Version -> Bool
psyNetIsU64 v =
  Version.major v >= 868 && Version.minor v >= 24 && Version.patch v >= 10

decodePsName :: BitGet.BitGet Text.Text
decodePsName = fmap
  (Text.dropWhileEnd (== '\x00') . Text.decodeLatin1 . reverseBytes)
  (BitGet.byteString 16)

decodePsBytes :: Version.Version -> BitGet.BitGet [Word.Word8]
decodePsBytes version = Bytes.unpack
  <$> BitGet.byteString (if playStationIsU24 version then 24 else 16)

playStationIsU24 :: Version.Version -> Bool
playStationIsU24 v =
  Version.major v >= 868 && Version.minor v >= 20 && Version.patch v >= 1

getWord256 :: BitGet.BitGet (U64.U64, U64.U64, U64.U64, U64.U64)
getWord256 = do
  a <- U64.bitGet
  b <- U64.bitGet
  c <- U64.bitGet
  d <- U64.bitGet
  pure (a, b, c, d)
