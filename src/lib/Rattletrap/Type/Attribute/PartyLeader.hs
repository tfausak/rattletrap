{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PartyLeader where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data PartyLeaderAttribute = PartyLeaderAttribute
  { systemId :: Word8le.Word8le
  , id :: Maybe (RemoteId.RemoteId, Word8le.Word8le)
  }
  deriving (Eq, Show)

$(deriveJsonWith ''PartyLeaderAttribute jsonOptions)

bitPut :: PartyLeaderAttribute -> BitPut ()
bitPut partyLeaderAttribute = do
  Word8le.bitPut (systemId partyLeaderAttribute)
  case Rattletrap.Type.Attribute.PartyLeader.id partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      RemoteId.bitPut remoteId
      Word8le.bitPut localId

bitGet
  :: (Int, Int, Int) -> BitGet PartyLeaderAttribute
bitGet version = do
  systemId_ <- Word8le.bitGet
  PartyLeaderAttribute systemId_ <$> decodeWhen
    (systemId_ /= Word8le.fromWord8 0)
    ((,) <$> RemoteId.bitGet version systemId_ <*> Word8le.bitGet)
