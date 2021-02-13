{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Section where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Crc
import Rattletrap.Decode.Common

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

-- | A section is a large piece of a 'Rattletrap.Replay.Replay'. It has a
-- 32-bit size (in bytes), a 32-bit CRC (see "Rattletrap.Utility.Crc"), and then a
-- bunch of data (the body). This interface is provided so that you don't have
-- to think about the size and CRC.
data Section a = Section
  { sectionSize :: Word32le
  -- ^ read only
  , sectionCrc :: Word32le
  -- ^ read only
  , sectionBody :: a
  -- ^ The actual content in the section.
  }
  deriving (Eq, Show)

$(deriveJson ''Section)

toSection :: (a -> Binary.Put) -> a -> Section a
toSection encode body =
  let bytes = LazyBytes.toStrict . Binary.runPut $ encode body
  in
    Section
      { sectionSize = Word32le . fromIntegral $ Bytes.length bytes
      , sectionCrc = Word32le $ getCrc32 bytes
      , sectionBody = body
      }

-- | Given a way to put the 'sectionBody', puts a section. This will also put
-- the size and CRC.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putSection' 'Rattletrap.Content.putContent' content)
-- @
putSection :: (a -> Binary.Put) -> Section a -> Binary.Put
putSection putBody section = do
  let
    rawBody =
      LazyBytes.toStrict (Binary.runPut (putBody (sectionBody section)))
  let size = Bytes.length rawBody
  let crc = getCrc32 rawBody
  putWord32 (Word32le (fromIntegral size))
  putWord32 (Word32le crc)
  Binary.putByteString rawBody

decodeSection :: Decode a -> Decode (Section a)
decodeSection getBody = do
  size <- decodeWord32le
  crc <- decodeWord32le
  rawBody <- getByteString (fromIntegral (word32leValue size))
  let actualCrc = Word32le (getCrc32 rawBody)
  Monad.when (actualCrc /= crc) (fail (crcMessage actualCrc crc))
  body <- either fail pure (runDecode getBody rawBody)
  pure (Section size crc body)

crcMessage :: Word32le -> Word32le -> String
crcMessage actual expected = unwords
  [ "[RT10] actual CRC"
  , show actual
  , "does not match expected CRC"
  , show expected
  ]
