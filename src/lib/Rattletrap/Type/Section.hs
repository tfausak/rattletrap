{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Section where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import qualified Rattletrap.Utility.Crc as Crc
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

-- | A section is a large piece of a 'Rattletrap.Replay.Replay'. It has a
-- 32-bit size (in bytes), a 32-bit CRC (see "Rattletrap.Utility.Crc"), and then a
-- bunch of data (the body). This interface is provided so that you don't have
-- to think about the size and CRC.
data Section a = Section
  { size :: Word32le.Word32le
  -- ^ read only
  , crc :: Word32le.Word32le
  -- ^ read only
  , body :: a
  -- ^ The actual content in the section.
  }
  deriving (Eq, Show)

$(deriveJson ''Section)

create :: (a -> BytePut) -> a -> Section a
create encode body_ =
  let bytes = LazyBytes.toStrict . Binary.runPut $ encode body_
  in
    Section
      { size = Word32le.fromWord32 . fromIntegral $ Bytes.length bytes
      , crc = Word32le.fromWord32 $ Crc.compute bytes
      , body = body_
      }

-- | Given a way to put the 'body', puts a section. This will also put
-- the size and CRC.
--
-- @
-- let bytes = 'Data.BytePut.runPut' ('bytePut' 'Rattletrap.Content.bytePut' content)
-- @
bytePut :: (a -> BytePut) -> Section a -> BytePut
bytePut putBody section = do
  let
    rawBody =
      LazyBytes.toStrict (Binary.runPut (putBody (body section)))
  let size_ = Bytes.length rawBody
  let crc_ = Crc.compute rawBody
  Word32le.bytePut (Word32le.fromWord32 (fromIntegral size_))
  Word32le.bytePut (Word32le.fromWord32 crc_)
  Binary.putByteString rawBody

byteGet :: ByteGet a -> ByteGet (Section a)
byteGet getBody = do
  size_ <- Word32le.byteGet
  crc_ <- Word32le.byteGet
  rawBody <- getByteString (fromIntegral (Word32le.toWord32 size_))
  let actualCrc = Word32le.fromWord32 (Crc.compute rawBody)
  Monad.when (actualCrc /= crc_) (fail (crcMessage actualCrc crc_))
  body_ <- either fail pure (runDecode getBody rawBody)
  pure (Section size_ crc_ body_)

crcMessage :: Word32le.Word32le -> Word32le.Word32le -> String
crcMessage actual expected = unwords
  [ "[RT10] actual CRC"
  , show actual
  , "does not match expected CRC"
  , show expected
  ]
