module Rattletrap.Decode.Section
  ( getSection
  ) where

import Rattletrap.Utility.Crc
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Word32le
import Rattletrap.Type.Section

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary

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
  rawBody <- Binary.getLazyByteString (fromIntegral (word32leValue size))
  let actualCrc = Word32le (getCrc32 rawBody)
  Monad.when (actualCrc /= crc) (fail (crcMessage actualCrc crc))
  let body = Binary.runGet getBody rawBody
  pure (Section size crc body)

crcMessage :: Word32le -> Word32le -> String
crcMessage actual expected = unwords
  ["actual CRC", show actual, "does not match expected CRC", show expected]
