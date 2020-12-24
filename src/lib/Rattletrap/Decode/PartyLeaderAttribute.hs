module Rattletrap.Decode.PartyLeaderAttribute
  ( decodePartyLeaderAttributeBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.RemoteId
import Rattletrap.Decode.Word8le
import Rattletrap.Type.PartyLeaderAttribute
import Rattletrap.Type.Word8le

decodePartyLeaderAttributeBits
  :: (Int, Int, Int) -> DecodeBits PartyLeaderAttribute
decodePartyLeaderAttributeBits version = do
  systemId <- decodeWord8leBits
  PartyLeaderAttribute systemId <$> decodeWhen
    (systemId /= Word8le 0)
    ((,) <$> decodeRemoteIdBits version systemId <*> decodeWord8leBits)
