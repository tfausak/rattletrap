module Rattletrap.Decode.PartyLeaderAttribute
  ( decodePartyLeaderAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.RemoteId
import Rattletrap.Decode.Word8le
import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Type.Word8le

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader

decodePartyLeaderAttributeBits
  :: Reader.ReaderT (Int, Int, Int) DecodeBits PartyLeaderAttribute
decodePartyLeaderAttributeBits = do
  systemId <- Trans.lift decodeWord8leBits
  PartyLeaderAttribute systemId <$> decodeWhen
    (systemId /= Word8le 0)
    ((,) <$> decodeRemoteIdBits systemId <*> Trans.lift decodeWord8leBits)
