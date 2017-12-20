module Rattletrap.Decode.UniqueIdAttribute
  ( decodeUniqueIdAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.RemoteId
import Rattletrap.Decode.Word8le
import Rattletrap.Type.UniqueIdAttribute

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader

decodeUniqueIdAttributeBits
  :: Reader.ReaderT (Int, Int, Int) DecodeBits UniqueIdAttribute
decodeUniqueIdAttributeBits = do
  systemId <- Trans.lift decodeWord8leBits
  UniqueIdAttribute systemId
    <$> decodeRemoteIdBits systemId
    <*> Trans.lift decodeWord8leBits
