module Rattletrap.Decode.ReservationAttribute
  ( decodeReservationAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.Str
import Rattletrap.Decode.UniqueIdAttribute
import Rattletrap.Type.ReservationAttribute
import Rattletrap.Type.UniqueIdAttribute
import Rattletrap.Type.Word8le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader

decodeReservationAttributeBits
  :: Reader.ReaderT (Int, Int, Int) DecodeBits ReservationAttribute
decodeReservationAttributeBits = do
  number <- Trans.lift (decodeCompressedWordBits 7)
  uniqueId <- decodeUniqueIdAttributeBits
  version <- Reader.ask
  ReservationAttribute number uniqueId
    <$> decodeWhen
          (uniqueIdAttributeSystemId uniqueId /= Word8le 0)
          (Trans.lift decodeStrBits)
    <*> Trans.lift getBool
    <*> Trans.lift getBool
    <*> decodeWhen (version >= (868, 12, 0)) (Trans.lift (getWord8Bits 6))
