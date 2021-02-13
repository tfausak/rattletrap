{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PartyLeader where

import Rattletrap.Type.Common
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8le
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8le)
  }
  deriving (Eq, Show)

$(deriveJson ''PartyLeaderAttribute)

putPartyLeaderAttribute :: PartyLeaderAttribute -> BinaryBits.BitPut ()
putPartyLeaderAttribute partyLeaderAttribute = do
  putWord8Bits (partyLeaderAttributeSystemId partyLeaderAttribute)
  case partyLeaderAttributeId partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      putRemoteId remoteId
      putWord8Bits localId

decodePartyLeaderAttributeBits
  :: (Int, Int, Int) -> BitGet PartyLeaderAttribute
decodePartyLeaderAttributeBits version = do
  systemId <- decodeWord8leBits
  PartyLeaderAttribute systemId <$> decodeWhen
    (systemId /= Word8le 0)
    ((,) <$> decodeRemoteIdBits version systemId <*> decodeWord8leBits)
