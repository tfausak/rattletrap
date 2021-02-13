{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Quaternion where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord

import qualified Data.Binary.Bits.Put as BinaryBits
import qualified Data.List as List
import qualified Data.Maybe as Maybe

data Quaternion = Quaternion
  { quaternionX :: Double
  , quaternionY :: Double
  , quaternionZ :: Double
  , quaternionW :: Double
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Quaternion)

data Component
  = ComponentX
  | ComponentY
  | ComponentZ
  | ComponentW
  deriving (Eq, Ord, Show)

toQuaternion :: Component -> Double -> Double -> Double -> Quaternion
toQuaternion component a b c =
  let d = toPart a b c
  in
    case component of
      ComponentX -> Quaternion d a b c
      ComponentY -> Quaternion a d b c
      ComponentZ -> Quaternion a b d c
      ComponentW -> Quaternion a b c d

toPart :: Double -> Double -> Double -> Double
toPart a b c = sqrt (1 - (a * a) - (b * b) - (c * c))

compressPart :: Double -> CompressedWord
compressPart =
  CompressedWord maxCompressedValue
    . round
    . (* wordToDouble maxCompressedValue)
    . (+ 0.5)
    . (/ 2.0)
    . (/ maxValue)

decompressPart :: CompressedWord -> Double
decompressPart x =
  (* maxValue)
    . (* 2.0)
    . subtract 0.5
    . (/ wordToDouble (compressedWordLimit x))
    . wordToDouble
    $ compressedWordValue x

maxComponent :: Quaternion -> Component
maxComponent quaternion =
  let
    x = quaternionX quaternion
    y = quaternionY quaternion
    z = quaternionZ quaternion
    w = quaternionW quaternion
    parts =
      [(x, ComponentX), (y, ComponentY), (z, ComponentZ), (w, ComponentW)]
    biggestPart = maximum parts
    roundTrip = decompressPart . compressPart
    computedPart = Maybe.fromMaybe
      biggestPart
      (List.find (\(value, _) -> value /= roundTrip value) parts)
  in snd
    (if (biggestPart == computedPart)
        || (abs (fst biggestPart - fst computedPart) > 0.00001)
      then biggestPart
      else computedPart
    )

numBits :: Word
numBits = 18

wordToDouble :: Word -> Double
wordToDouble = fromIntegral

maxCompressedValue :: Word
maxCompressedValue = (2 ^ numBits) - 1

maxValue :: Double
maxValue = 1.0 / sqrt 2.0

putQuaternion :: Quaternion -> BinaryBits.BitPut ()
putQuaternion q = do
  let c = maxComponent q
  putComponent c
  case c of
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
putPart = putCompressedWord . compressPart
