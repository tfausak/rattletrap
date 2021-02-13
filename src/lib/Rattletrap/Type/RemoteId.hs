{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.RemoteId where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word64le as Word64le
import Rattletrap.Encode.Common
import Rattletrap.Utility.Bytes
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.Word8le as Word8le

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word
import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.ByteString as Bytes

data RemoteId
  = PlayStation Text [Word8]
  | PsyNet (Either Word64le.Word64le (Word64le.Word64le, Word64le.Word64le, Word64le.Word64le, Word64le.Word64le))
  | Splitscreen Word32
  -- ^ Really only 24 bits.
  | Steam Word64le.Word64le
  | Switch Word64le.Word64le Word64le.Word64le Word64le.Word64le Word64le.Word64le
  | Xbox Word64le.Word64le
  | Epic Str.Str
  deriving (Eq, Show)

$(deriveJsonWith ''RemoteId jsonOptions)

bitPut :: RemoteId -> BitPut ()
bitPut remoteId = case remoteId of
  PlayStation name bytes -> do
    let rawName = reverseBytes (padBytes (16 :: Int) (encodeLatin1 name))
    BinaryBits.putByteString rawName
    BinaryBits.putByteString (Bytes.pack bytes)
  PsyNet e -> case e of
    Left l -> Word64le.bitPut l
    Right (a, b, c, d) -> putWord256 a b c d
  Splitscreen word24 -> putBitsLE 24 word24
  Steam word64 -> Word64le.bitPut word64
  Switch a b c d -> putWord256 a b c d
  Xbox word64 -> Word64le.bitPut word64
  Epic str -> Str.bitPut str

putWord256
  :: Word64le.Word64le -> Word64le.Word64le -> Word64le.Word64le -> Word64le.Word64le -> BitPut ()
putWord256 a b c d = do
  Word64le.bitPut a
  Word64le.bitPut b
  Word64le.bitPut c
  Word64le.bitPut d

bitGet :: (Int, Int, Int) -> Word8le.Word8le -> BitGet RemoteId
bitGet version systemId = case Word8le.toWord8 systemId of
  0 -> Splitscreen <$> getBitsLE 24
  1 -> Steam <$> Word64le.bitGet
  2 -> PlayStation <$> decodePsName <*> decodePsBytes version
  4 -> Xbox <$> Word64le.bitGet
  6 -> do
    (a, b, c, d) <- getWord256
    pure $ Switch a b c d
  7 -> if version >= (868, 24, 10)
    then PsyNet . Left <$> Word64le.bitGet
    else PsyNet . Right <$> getWord256
  11 -> Epic <$> Str.bitGet
  _ -> fail ("[RT09] unknown system id " <> show systemId)

decodePsName :: BitGet Text.Text
decodePsName = fmap
  (Text.dropWhileEnd (== '\x00') . Text.decodeLatin1 . reverseBytes)
  (getByteStringBits 16)

decodePsBytes :: (Int, Int, Int) -> BitGet [Word.Word8]
decodePsBytes version = Bytes.unpack
  <$> getByteStringBits (if version >= (868, 20, 1) then 24 else 16)

getWord256 :: BitGet (Word64le.Word64le, Word64le.Word64le, Word64le.Word64le, Word64le.Word64le)
getWord256 = do
  a <- Word64le.bitGet
  b <- Word64le.bitGet
  c <- Word64le.bitGet
  d <- Word64le.bitGet
  pure (a, b, c, d)
