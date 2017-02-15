module Rattletrap.Primitive.Section where

import Rattletrap.Crc
import Rattletrap.Primitive.Word32

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

-- | A section is a large piece of a 'Rattletrap.Replay.Replay'. It has a
-- 32-bit size (in bytes), a 32-bit CRC (see "Rattletrap.Crc"), and then a
-- bunch of data (the body). This interface is provided so that you don't have
-- to think about the size and CRC.
newtype Section a = Section
  { sectionBody :: a
  -- ^ The actual content in the section.
  } deriving (Eq, Show)

-- | Given a way to get the 'sectionBody', gets a section. This will fail if
-- the section's CRC does not match.
--
-- @
-- let header = 'Data.Binary.Get.runGet' ('getSection' 'Rattletrap.Header.getHeader') bytes
-- @
getSection :: Binary.Get a -> Binary.Get (Section a)
getSection getBody = do
  size <- getWord32
  crc <- getWord32
  rawBody <- Binary.getLazyByteString (fromIntegral (word32Value size))
  let actualCrc = Word32 (getCrc32 rawBody)
  Monad.when (actualCrc /= crc) (fail (crcMessage actualCrc crc))
  let body = Binary.runGet getBody rawBody
  pure (Section body)

-- | Given a way to put the 'sectionBody', puts a section. This will also put
-- the size and CRC.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putSection' 'Rattletrap.Content.putContent' content)
-- @
putSection :: (a -> Binary.Put) -> Section a -> Binary.Put
putSection putBody section = do
  let rawBody = Binary.runPut (putBody (sectionBody section))
  let size = ByteString.length rawBody
  let crc = getCrc32 rawBody
  putWord32 (Word32 (fromIntegral size))
  putWord32 (Word32 crc)
  Binary.putLazyByteString rawBody

crcMessage :: Word32 -> Word32 -> String
crcMessage actual expected =
  unwords
    ["actual CRC", show actual, "does not match expected CRC", show expected]
