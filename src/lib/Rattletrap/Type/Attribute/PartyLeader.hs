{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PartyLeader where

import Rattletrap.Type.Common
import Rattletrap.Type.RemoteId
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data PartyLeaderAttribute = PartyLeaderAttribute
  { partyLeaderAttributeSystemId :: Word8le.Word8le
  , partyLeaderAttributeId :: Maybe (RemoteId, Word8le.Word8le)
  }
  deriving (Eq, Show)

$(deriveJson ''PartyLeaderAttribute)

putPartyLeaderAttribute :: PartyLeaderAttribute -> BitPut ()
putPartyLeaderAttribute partyLeaderAttribute = do
  Word8le.bitPut (partyLeaderAttributeSystemId partyLeaderAttribute)
  case partyLeaderAttributeId partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      putRemoteId remoteId
      Word8le.bitPut localId

decodePartyLeaderAttributeBits
  :: (Int, Int, Int) -> BitGet PartyLeaderAttribute
decodePartyLeaderAttributeBits version = do
  systemId <- Word8le.bitGet
  PartyLeaderAttribute systemId <$> decodeWhen
    (systemId /= Word8le.fromWord8 0)
    ((,) <$> decodeRemoteIdBits version systemId <*> Word8le.bitGet)
