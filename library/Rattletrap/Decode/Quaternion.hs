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
    y -> fail ("invalid component: " <> show y)

decodePart :: DecodeBits Double
decodePart =
  (* maxValueDouble)
    . (* 2.0)
    . subtract 0.5
    . (/ wordToDouble maxValueWord)
    . wordToDouble
    . compressedWordValue
    <$> decodeCompressedWordBits (maxValueWord + 1)

numBits :: Word
numBits = 18

wordToDouble :: Word -> Double
wordToDouble = fromIntegral

maxValueWord :: Word
maxValueWord = (2 ^ numBits) - 1

maxValueDouble :: Double
maxValueDouble = 1.0 / sqrt 2.0
