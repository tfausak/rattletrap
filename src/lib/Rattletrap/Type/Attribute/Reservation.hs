module Rattletrap.Type.Attribute.Reservation where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.UniqueId as UniqueId
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import Rattletrap.Utility.Monad

data Reservation = Reservation
  { number :: CompressedWord.CompressedWord
  , uniqueId :: UniqueId.UniqueId
  , name :: Maybe Str.Str
  , unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: Maybe Word.Word8
  }
  deriving (Eq, Show)

instance Json.FromJSON Reservation where
  parseJSON = Json.withObject "Reservation" $ \object -> do
    number <- Json.required object "number"
    uniqueId <- Json.required object "unique_id"
    name <- Json.optional object "name"
    unknown1 <- Json.required object "unknown1"
    unknown2 <- Json.required object "unknown2"
    unknown3 <- Json.optional object "unknown3"
    pure Reservation { number, uniqueId, name, unknown1, unknown2, unknown3 }

instance Json.ToJSON Reservation where
  toJSON x = Json.object
    [ Json.pair "number" $ number x
    , Json.pair "unique_id" $ uniqueId x
    , Json.pair "name" $ name x
    , Json.pair "unknown1" $ unknown1 x
    , Json.pair "unknown2" $ unknown2 x
    , Json.pair "unknown3" $ unknown3 x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute-reservation" $ Schema.object
  [ (Json.pair "number" $ Schema.ref CompressedWord.schema, True)
  , (Json.pair "unique_id" $ Schema.ref UniqueId.schema, True)
  , (Json.pair "name" . Schema.json $ Schema.maybe Str.schema, False)
  , (Json.pair "unknown1" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown2" $ Schema.ref Schema.boolean, True)
  , (Json.pair "unknown3" . Schema.json $ Schema.maybe Schema.integer, False)
  ]

bitPut :: Reservation -> BitPut.BitPut
bitPut reservationAttribute =
  CompressedWord.bitPut (number reservationAttribute)
    <> UniqueId.bitPut (uniqueId reservationAttribute)
    <> foldMap Str.bitPut (name reservationAttribute)
    <> BitPut.bool (unknown1 reservationAttribute)
    <> BitPut.bool (unknown2 reservationAttribute)
    <> foldMap (BitPut.word8 6) (unknown3 reservationAttribute)

bitGet :: Version.Version -> BitGet.BitGet Reservation
bitGet version = do
  number <- CompressedWord.bitGet 7
  uniqueId <- UniqueId.bitGet version
  name <- whenMaybe (UniqueId.systemId uniqueId /= U8.fromWord8 0) Str.bitGet
  unknown1 <- BitGet.bool
  unknown2 <- BitGet.bool
  unknown3 <- whenMaybe (hasUnknown3 version) (BitGet.word8 6)
  pure Reservation { number, uniqueId, name, unknown1, unknown2, unknown3 }

hasUnknown3 :: Version.Version -> Bool
hasUnknown3 v =
  Version.major v >= 868 && Version.minor v >= 12 && Version.patch v >= 0
