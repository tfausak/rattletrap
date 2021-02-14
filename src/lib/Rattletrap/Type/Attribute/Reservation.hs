{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Reservation where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Attribute.UniqueId as UniqueId
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.BitPut as BitPut

data Reservation = Reservation
  { number :: CompressedWord.CompressedWord
  , uniqueId :: UniqueId.UniqueId
  , name :: Maybe Str.Str
  , unknown1 :: Bool
  , unknown2 :: Bool
  , unknown3 :: Maybe Word8
  }
  deriving (Eq, Show)

$(deriveJson ''Reservation)

bitPut :: Reservation -> BitPut.BitPut
bitPut reservationAttribute = do
  CompressedWord.bitPut (number reservationAttribute)
  UniqueId.bitPut (uniqueId reservationAttribute)
  case name reservationAttribute of
    Nothing -> pure ()
    Just name_ -> Str.bitPut name_
  BitPut.bool (unknown1 reservationAttribute)
  BitPut.bool (unknown2 reservationAttribute)
  case unknown3 reservationAttribute of
    Nothing -> pure ()
    Just c -> BitPut.word8 6 c

bitGet
  :: (Int, Int, Int) -> BitGet Reservation
bitGet version = do
  number_ <- CompressedWord.bitGet 7
  uniqueId_ <- UniqueId.bitGet version
  Reservation number_ uniqueId_
    <$> decodeWhen
          (UniqueId.systemId uniqueId_ /= U8.fromWord8 0)
          Str.bitGet
    <*> getBool
    <*> getBool
    <*> decodeWhen (version >= (868, 12, 0)) (getWord8Bits 6)
