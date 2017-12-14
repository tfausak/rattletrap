module Rattletrap.Decode.PartyLeaderAttribute
  ( getPartyLeaderAttribute
  ) where

import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.RemoteId

import qualified Data.Binary.Bits.Get as BinaryBit

getPartyLeaderAttribute :: (Int, Int, Int) -> BinaryBit.BitGet PartyLeaderAttribute
getPartyLeaderAttribute version = do
  systemId <- getWord8Bits
  maybeRemoteAndLocalId <- if systemId == Word8 0
    then pure Nothing
    else do
      remoteId <- getRemoteId version systemId
      localId <- getWord8Bits
      pure (Just (remoteId, localId))
  pure (PartyLeaderAttribute systemId maybeRemoteAndLocalId)
