module Rattletrap.Type.Attribute.PartyLeader where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.RemoteId as RemoteId
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Utility.Monad

data PartyLeader = PartyLeader
  { systemId :: U8.U8
  , id :: Maybe (RemoteId.RemoteId, U8.U8)
  }
  deriving (Eq, Show)

$(deriveJson ''PartyLeader)

bitPut :: PartyLeader -> BitPut.BitPut
bitPut x = U8.bitPut (systemId x) <> foldMap
  (\(y, z) -> RemoteId.bitPut y <> U8.bitPut z)
  (Rattletrap.Type.Attribute.PartyLeader.id x)

bitGet :: (Int, Int, Int) -> BitGet.BitGet PartyLeader
bitGet version = do
  systemId_ <- U8.bitGet
  PartyLeader systemId_ <$> whenMaybe
    (systemId_ /= U8.fromWord8 0)
    ((,) <$> RemoteId.bitGet version systemId_ <*> U8.bitGet)
