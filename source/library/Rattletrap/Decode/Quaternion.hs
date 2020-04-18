module Rattletrap.Decode.Quaternion
  ( decodeQuaternionBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Quaternion

decodeQuaternionBits :: DecodeBits Quaternion
decodeQuaternionBits =
  toQuaternion <$> decodeComponent <*> decodePart <*> decodePart <*> decodePart

decodeComponent :: DecodeBits Component
decodeComponent = do
  x <- decodeCompressedWordBits 3
  case compressedWordValue x of
    0 -> pure ComponentX
    1 -> pure ComponentY
    2 -> pure ComponentZ
    3 -> pure ComponentW
    y -> fail ("[RT08] invalid component: " <> show y)

decodePart :: DecodeBits Double
decodePart = decompressPart <$> decodeCompressedWordBits maxCompressedValue
