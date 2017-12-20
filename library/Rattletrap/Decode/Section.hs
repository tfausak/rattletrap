module Rattletrap.Decode.Section
  ( decodeSection
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Section
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Crc

import qualified Control.Monad as Monad

decodeSection :: Decode a -> Decode (Section a)
decodeSection getBody = do
  size <- decodeWord32le
  crc <- decodeWord32le
  rawBody <- getLazyByteString (fromIntegral (word32leValue size))
  let actualCrc = Word32le (getCrc32 rawBody)
  Monad.when (actualCrc /= crc) (fail (crcMessage actualCrc crc))
  case runGetOrFail getBody rawBody of
    Left (_, _, problem) -> fail problem
    Right (_, _, body) -> pure (Section size crc body)

crcMessage :: Word32le -> Word32le -> String
crcMessage actual expected = unwords
  ["actual CRC", show actual, "does not match expected CRC", show expected]
