module Rattletrap.Type.Name where

import qualified Data.Aeson as Aeson
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str

-- | This is a name like @"VehiclePickup_Boost_TA_15"@.
newtype Name = MkName
  { unwrap :: Str.Str
  }
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON Name where
  parseJSON = fmap MkName . Aeson.parseJSON

instance Aeson.ToJSON Name where
  toJSON = Aeson.toJSON . unwrap

bytePut :: Name -> BytePut.BytePut
bytePut = Str.bytePut . unwrap

byteGet :: ByteGet.ByteGet Name
byteGet = fmap MkName Str.byteGet
