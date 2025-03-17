module Rattletrap.Type.ObjectName where

import qualified Data.Aeson as Aeson
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str

-- | This can either be an object name like @"TAGame.VehiclePickup_TA"@ or an
-- attribute name like @"TAGame.VehiclePickup_TA:ReplicatedPickupData"@.
newtype ObjectName = MkObjectName
  { unwrap :: Str.Str
  }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON ObjectName where
  parseJSON = fmap MkObjectName . Aeson.parseJSON

instance Aeson.ToJSON ObjectName where
  toJSON = Aeson.toJSON . unwrap

bytePut :: ObjectName -> BytePut.BytePut
bytePut = Str.bytePut . unwrap

byteGet :: ByteGet.ByteGet ObjectName
byteGet = fmap MkObjectName Str.byteGet
