module Rattletrap.Type.RemoteId.PlayStation where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Bytes as Bytes
import qualified Rattletrap.Vendor.Argo as Argo

data PlayStation = PlayStation
  { name :: Text.Text
  , code :: [Word.Word8]
  }
  deriving (Eq, Show)

instance Argo.HasCodec PlayStation where
  codec =
    Argo.fromArrayCodec Argo.Allow
      $ PlayStation
      <$> Argo.project name (Argo.element Argo.codec)
      <*> Argo.project code (Argo.element Argo.codec)

bitPut :: PlayStation -> BitPut.BitPut
bitPut x =
  let
    nameBytes = Bytes.padBytes (16 :: Int) . Bytes.encodeLatin1 $ name x
    codeBytes = ByteString.pack $ code x
  in BitPut.byteString nameBytes <> BitPut.byteString codeBytes

bitGet :: Version.Version -> BitGet.BitGet PlayStation
bitGet version = BitGet.label "PlayStation" $ do
  name <- BitGet.label "name" getCode
  code <- BitGet.label "code" $ getName version
  pure PlayStation { name, code }

getCode :: BitGet.BitGet Text.Text
getCode = fmap (Text.dropWhileEnd (== '\x00') . Text.decodeLatin1)
  $ BitGet.byteString 16

getName :: Version.Version -> BitGet.BitGet [Word.Word8]
getName version =
  fmap ByteString.unpack
    . BitGet.byteString
    $ if Version.atLeast 868 20 1 version then 24 else 16
