module Rattletrap.Section where

import Rattletrap.Crc
import Rattletrap.Primitive

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

newtype Section a = Section
  { sectionBody :: a
  } deriving (Eq, Ord, Show)

getSection :: Binary.Get a -> Binary.Get (Section a)
getSection getBody = do
  size <- getWord32
  crc <- getWord32
  rawBody <- Binary.getLazyByteString (fromIntegral (word32Value size))
  let actualCrc = Word32 (getCrc32 rawBody)
  Monad.when (actualCrc /= crc) (fail (crcMessage actualCrc crc))
  let body = Binary.runGet getBody rawBody
  pure (Section body)

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
