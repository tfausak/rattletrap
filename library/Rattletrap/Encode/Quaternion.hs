module Rattletrap.Encode.Quaternion
  ( putQuaternion
  )
where

import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Quaternion

import qualified Data.Binary.Bits.Put as BinaryBits

putQuaternion :: Quaternion -> BinaryBits.BitPut ()
putQuaternion q = do
  putComponent (maxComponent q)
  case maxComponent q of
    ComponentX -> putParts (quaternionY q) (quaternionZ q) (quaternionW q)
    ComponentY -> putParts (quaternionX q) (quaternionZ q) (quaternionW q)
    ComponentZ -> putParts (quaternionX q) (quaternionY q) (quaternionW q)
    ComponentW -> putParts (quaternionX q) (quaternionY q) (quaternionZ q)

putComponent :: Component -> BinaryBits.BitPut ()
putComponent component = putCompressedWord
  (CompressedWord
    3
    (case component of
      ComponentX -> 0
      ComponentY -> 1
      ComponentZ -> 2
      ComponentW -> 3
    )
  )

putParts :: Double -> Double -> Double -> BinaryBits.BitPut ()
putParts a b c = do
  putPart a
  putPart b
  putPart c

putPart :: Double -> BinaryBits.BitPut ()
putPart =
  putCompressedWord
    . CompressedWord (maxValueWord + 1)
    . round
    . (* wordToDouble maxValueWord)
    . (+ 0.5)
    . (/ 2.0)
    . (/ maxValueDouble)

numBits :: Word
numBits = 18

wordToDouble :: Word -> Double
wordToDouble = fromIntegral

maxValueWord :: Word
maxValueWord = (2 ^ numBits) - 1

maxValueDouble :: Double
maxValueDouble = 1.0 / sqrt 2.0
