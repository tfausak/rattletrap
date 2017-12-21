module Rattletrap.Encode.Section
  ( putSection
  ) where

import Rattletrap.Encode.Word32le
import Rattletrap.Type.Section
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Crc

import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes

-- | Given a way to put the 'sectionBody', puts a section. This will also put
-- the size and CRC.
--
-- @
-- let bytes = 'Data.Binary.Put.runPut' ('putSection' 'Rattletrap.Content.putContent' content)
-- @
putSection :: (a -> Binary.Put) -> Section a -> Binary.Put
putSection putBody section = do
  let rawBody = Binary.runPut (putBody (sectionBody section))
  let size = LazyBytes.length rawBody
  let crc = getCrc32 rawBody
  putWord32 (Word32le (fromIntegral size))
  putWord32 (Word32le crc)
  Binary.putLazyByteString rawBody
