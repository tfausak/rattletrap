{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.PartyLeader where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data PartyLeaderAttribute = PartyLeaderAttribute
  { systemId :: U8.U8
  , id :: Maybe (RemoteId.RemoteId, U8.U8)
  }
  deriving (Eq, Show)

$(deriveJson ''PartyLeaderAttribute)

bitPut :: PartyLeaderAttribute -> BitPut ()
bitPut partyLeaderAttribute = do
  U8.bitPut (systemId partyLeaderAttribute)
  case Rattletrap.Type.Attribute.PartyLeader.id partyLeaderAttribute of
    Nothing -> pure ()
    Just (remoteId, localId) -> do
      RemoteId.bitPut remoteId
      U8.bitPut localId

bitGet
  :: (Int, Int, Int) -> BitGet PartyLeaderAttribute
bitGet version = do
  systemId_ <- U8.bitGet
  PartyLeaderAttribute systemId_ <$> decodeWhen
    (systemId_ /= U8.fromWord8 0)
    ((,) <$> RemoteId.bitGet version systemId_ <*> U8.bitGet)
