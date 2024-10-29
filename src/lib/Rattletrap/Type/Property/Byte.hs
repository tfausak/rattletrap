module Rattletrap.Type.Property.Byte where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Utility.Json as Json

data Byte = Byte
  { key :: Str.Str,
    value :: Maybe (Either U8.U8 Str.Str)
  }
  deriving (Eq, Show)

instance Json.FromJSON Byte where
  parseJSON json = do
    (key, value) <- Json.parseJSON json
    pure Byte {key, value}

instance Json.ToJSON Byte where
  toJSON byte = Json.toJSON (key byte, value byte)

schema :: Schema.Schema
schema =
  Schema.named "property-byte" $
    Schema.tuple
      [ Schema.ref Str.schema,
        Schema.oneOf
          [ Schema.ref Schema.null,
            Schema.object [(Json.pair "Left" $ Schema.ref U8.schema, True)],
            Schema.object [(Json.pair "Right" $ Schema.ref Str.schema, True)]
          ]
      ]

bytePut :: Byte -> BytePut.BytePut
bytePut byte = Str.bytePut (key byte) <> foldMap (either U8.bytePut Str.bytePut) (value byte)

byteGet :: ByteGet.ByteGet Byte
byteGet = ByteGet.label "Byte" $ do
  key <- ByteGet.label "key" Str.byteGet
  let isSteam = key == Str.fromString "OnlinePlatform_Steam"
      isPlayStation = key == Str.fromString "OnlinePlatform_PS4"
      isNone = key == Str.fromString "None"
  value <-
    ByteGet.label "value" $
      if isSteam || isPlayStation
        then pure Nothing
        else
          if isNone
            then Just . Left <$> U8.byteGet
            else Just . Right <$> Str.byteGet
  pure Byte {key, value}
