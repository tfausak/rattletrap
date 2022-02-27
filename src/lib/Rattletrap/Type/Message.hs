module Rattletrap.Type.Message where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data Message = Message
  { frame :: U32.U32
  -- ^ Which frame this message belongs to, starting from 0.
  , name :: Str.Str
  -- ^ The primary player's name.
  , value :: Str.Str
  -- ^ The content of the message.
  }
  deriving (Eq, Show)

instance Argo.HasCodec Message where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ Message
      <$> Argo.required frame "frame"
      <*> Argo.required name "name"
      <*> Argo.required value "value"

bytePut :: Message -> BytePut.BytePut
bytePut x =
  U32.bytePut (frame x) <> Str.bytePut (name x) <> Str.bytePut (value x)

byteGet :: ByteGet.ByteGet Message
byteGet = ByteGet.label "Message" $ do
  frame <- ByteGet.label "frame" U32.byteGet
  name <- ByteGet.label "name" Str.byteGet
  value <- ByteGet.label "value" Str.byteGet
  pure Message { frame, name, value }
