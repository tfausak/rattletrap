module Rattletrap.Type.RemoteId.PlayStation where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Bytes as Bytes
import qualified Rattletrap.Utility.Json as Json

data PlayStation = PlayStation
  { name :: Text.Text
  , code :: [Word.Word8]
  }
  deriving (Eq, Show)

instance Json.FromJSON PlayStation where
  parseJSON json = do
    (name, code) <- Json.parseJSON json
    pure PlayStation { name, code }

instance Json.ToJSON PlayStation where
  toJSON x = Json.toJSON (name x, code x)

schema :: Schema.Schema
schema = Schema.named "remote-id-play-station" $ Schema.tuple
  [Schema.ref Schema.string, Schema.json $ Schema.array Schema.number]

bitPut :: PlayStation -> BitPut.BitPut
bitPut x =
  let
    nameBytes = Bytes.padBytes (16 :: Int) . Bytes.encodeLatin1 $ name x
    codeBytes = ByteString.pack $ code x
  in BitPut.byteString nameBytes <> BitPut.byteString codeBytes

bitGet :: Version.Version -> BitGet.BitGet PlayStation
bitGet version = do
  name <- getCode
  code <- getName version
  pure PlayStation { name, code }

getCode :: BitGet.BitGet Text.Text
getCode = fmap (Text.dropWhileEnd (== '\x00') . Text.decodeLatin1)
  $ BitGet.byteString 16

getName :: Version.Version -> BitGet.BitGet [Word.Word8]
getName version =
  fmap ByteString.unpack
    . BitGet.byteString
    $ if Version.atLeast 868 20 1 version then 24 else 16
