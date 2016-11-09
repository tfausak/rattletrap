module Rattletrap.Section where

import Rattletrap.Crc
import Rattletrap.Word32

import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString

newtype Section a = Section
  { sectionBody :: a
  } deriving (Eq, Ord, Show)

getSection :: Binary.Get a -> Binary.Get (Section a)
getSection getBody = do
  _size <- getWord32
  _crc <- getWord32
  body <- getBody
  pure (Section body)

putSection :: (a -> Binary.Put) -> Section a -> Binary.Put
putSection putBody section = do
  let rawBody = Binary.runPut (putBody (sectionBody section))
  let size = ByteString.length rawBody
  let crc = getCrc32 rawBody
  putWord32 (Word32 (fromIntegral size))
  putWord32 (Word32 crc)
  Binary.putLazyByteString rawBody
