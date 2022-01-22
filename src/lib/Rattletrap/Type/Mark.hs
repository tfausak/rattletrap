module Rattletrap.Type.Mark where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Vendor.Argo as Argo

data Mark = Mark
  { value :: Str.Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , frame :: U32.U32
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Show)

instance Argo.HasCodec Mark where
  codec =
    Argo.fromObjectCodec Argo.Allow
      $ Mark
      <$> Argo.project
            value
            (Argo.required (Argo.fromString "value") Argo.codec)
      <*> Argo.project
            frame
            (Argo.required (Argo.fromString "frame") Argo.codec)

bytePut :: Mark -> BytePut.BytePut
bytePut x = Str.bytePut (value x) <> U32.bytePut (frame x)

byteGet :: ByteGet.ByteGet Mark
byteGet = ByteGet.label "Mark" $ do
  value <- ByteGet.label "value" Str.byteGet
  frame <- ByteGet.label "frame" U32.byteGet
  pure Mark { value, frame }
