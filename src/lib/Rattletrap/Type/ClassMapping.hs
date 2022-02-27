module Rattletrap.Type.ClassMapping where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data ClassMapping = ClassMapping
  { name :: Str.Str
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

instance Argo.HasCodec ClassMapping where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ ClassMapping
      <$> Argo.project name (Argo.required "name" Argo.codec)
      <*> Argo.project streamId (Argo.required "stream_id" Argo.codec)

bytePut :: ClassMapping -> BytePut.BytePut
bytePut x = Str.bytePut (name x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet ClassMapping
byteGet = ByteGet.label "ClassMapping" $ do
  name <- ByteGet.label "name" Str.byteGet
  streamId <- ByteGet.label "streamId" U32.byteGet
  pure ClassMapping { name, streamId }
