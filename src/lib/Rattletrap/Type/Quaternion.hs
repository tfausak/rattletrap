{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Quaternion where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord

data Quaternion = Quaternion
  { x :: Double
  , y :: Double
  , z :: Double
  , w :: Double
  }
  deriving (Eq, Show)

$(deriveJson ''Quaternion)

data Component
  = X
  | Y
  | Z
  | W
  deriving (Eq, Show)

toQuaternion :: Component -> Double -> Double -> Double -> Quaternion
toQuaternion component a b c =
  let d = toPart a b c
  in
    case component of
      X -> Quaternion d a b c
      Y -> Quaternion a d b c
      Z -> Quaternion a b d c
      W -> Quaternion a b c d

toPart :: Double -> Double -> Double -> Double
toPart a b c = sqrt (1 - (a * a) - (b * b) - (c * c))

compressPart :: Double -> CompressedWord.CompressedWord
compressPart =
  CompressedWord.CompressedWord maxCompressedValue
    . round
    . (* wordToDouble maxCompressedValue)
    . (+ 0.5)
    . (/ 2.0)
    . (/ maxValue)

decompressPart :: CompressedWord.CompressedWord -> Double
decompressPart x_ =
  (* maxValue)
    . (* 2.0)
    . subtract 0.5
    . (/ wordToDouble (CompressedWord.limit x_))
    . wordToDouble
    $ CompressedWord.value x_

maxComponent :: Quaternion -> Component
maxComponent quaternion =
  let
    x_ = x quaternion
    y_ = y quaternion
    z_ = z quaternion
    w_ = w quaternion
    parts = [(x_, X), (y_, Y), (z_, Z), (w_, W)]
    biggestPart = maximumOn fst parts
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

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = List.maximumBy (Ord.comparing f)

numBits :: Word
numBits = 18

wordToDouble :: Word -> Double
wordToDouble = fromIntegral

maxCompressedValue :: Word
maxCompressedValue = (2 ^ numBits) - 1

maxValue :: Double
maxValue = 1.0 / sqrt 2.0

bitPut :: Quaternion -> BitPut.BitPut
bitPut q =
  let c = maxComponent q
  in
    putComponent c <> case c of
      X -> putParts (y q) (z q) (w q)
      Y -> putParts (x q) (z q) (w q)
      Z -> putParts (x q) (y q) (w q)
      W -> putParts (x q) (y q) (z q)

putComponent :: Component -> BitPut.BitPut
putComponent component = CompressedWord.bitPut
  (CompressedWord.CompressedWord
    3
    (case component of
      X -> 0
      Y -> 1
      Z -> 2
      W -> 3
    )
  )

putParts :: Double -> Double -> Double -> BitPut.BitPut
putParts a b c = putPart a <> putPart b <> putPart c

putPart :: Double -> BitPut.BitPut
putPart = CompressedWord.bitPut . compressPart

bitGet :: BitGet.BitGet Quaternion
bitGet =
  toQuaternion <$> decodeComponent <*> decodePart <*> decodePart <*> decodePart

decodeComponent :: BitGet.BitGet Component
decodeComponent = do
  x_ <- CompressedWord.bitGet 3
  case CompressedWord.value x_ of
    0 -> pure X
    1 -> pure Y
    2 -> pure Z
    3 -> pure W
    y_ -> fail ("[RT08] invalid component: " <> show y_)

decodePart :: BitGet.BitGet Double
decodePart = decompressPart <$> CompressedWord.bitGet maxCompressedValue
