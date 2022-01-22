module Rattletrap.Type.Attribute.Reservation where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.UniqueId as UniqueId
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data Reservation = Reservation
  { number :: CompressedWord.CompressedWord
  , uniqueId :: UniqueId.UniqueId
  , name :: Maybe Str.Str
  , unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: Maybe Word.Word8
  }
  deriving (Eq, Show)

instance Argo.HasCodec Reservation where
  codec = Argo.fromObjectCodec Argo.Allow $ Reservation
    <$> Argo.project number (Argo.required (Argo.fromString "number") Argo.codec)
    <*> Argo.project uniqueId (Argo.required (Argo.fromString "unique_id") Argo.codec)
    <*> Argo.project name (Argo.optional (Argo.fromString "name") Argo.codec)
    <*> Argo.project unknown1 (Argo.required (Argo.fromString "unknow1n") Argo.codec)
    <*> Argo.project unknown2 (Argo.required (Argo.fromString "unknown2") Argo.codec)
    <*> Argo.project unknown3 (Argo.required (Argo.fromString "unknown3") Argo.codec)

bitPut :: Reservation -> BitPut.BitPut
bitPut reservationAttribute =
  CompressedWord.bitPut (number reservationAttribute)
    <> UniqueId.bitPut (uniqueId reservationAttribute)
    <> foldMap Str.bitPut (name reservationAttribute)
    <> BitPut.bool (unknown1 reservationAttribute)
    <> BitPut.bool (unknown2 reservationAttribute)
    <> foldMap (BitPut.bits 6) (unknown3 reservationAttribute)

bitGet :: Version.Version -> BitGet.BitGet Reservation
bitGet version = BitGet.label "Reservation" $ do
  number <- BitGet.label "number" $ CompressedWord.bitGet 7
  uniqueId <- BitGet.label "uniqueId" $ UniqueId.bitGet version
  name <- BitGet.label "name" $ Monad.whenMaybe
    (UniqueId.systemId uniqueId /= U8.fromWord8 0)
    Str.bitGet
  unknown1 <- BitGet.label "unknown1" BitGet.bool
  unknown2 <- BitGet.label "unknown2" BitGet.bool
  unknown3 <-
    BitGet.label "unknown3"
    . Monad.whenMaybe (Version.atLeast 868 12 0 version)
    $ BitGet.bits 6
  pure Reservation { number, uniqueId, name, unknown1, unknown2, unknown3 }
