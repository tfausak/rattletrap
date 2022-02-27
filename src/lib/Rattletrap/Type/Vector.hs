module Rattletrap.Type.Vector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Vector = Vector
  { size :: CompressedWord.CompressedWord
  , bias :: Word
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Word' rather than something more precise like a
  -- 'Word8' because it just gets passed to a functions that expect 'Word's.
  -- There's no reason to do a bunch of conversions.
  , x :: Int
  -- ^ See 'bias'.
  , y :: Int
  -- ^ See 'bias'.
  , z :: Int
  -- ^ See 'bias'.
  }
  deriving (Eq, Show)

instance Argo.HasCodec Vector where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Vector
      <$> Argo.required size "size"
      <*> Argo.required bias "bias"
      <*> Argo.required x "x"
      <*> Argo.required y "y"
      <*> Argo.required z "z"

bitPut :: Vector -> BitPut.BitPut
bitPut vector =
  let
    bitSize =
      round (logBase (2 :: Float) (fromIntegral (bias vector))) - 1 :: Word
    dx = fromIntegral (x vector + fromIntegral (bias vector)) :: Word
    dy = fromIntegral (y vector + fromIntegral (bias vector)) :: Word
    dz = fromIntegral (z vector + fromIntegral (bias vector)) :: Word
    limit = 2 ^ (bitSize + 2) :: Word
  in
    CompressedWord.bitPut (size vector)
    <> CompressedWord.bitPut (CompressedWord.CompressedWord limit dx)
    <> CompressedWord.bitPut (CompressedWord.CompressedWord limit dy)
    <> CompressedWord.bitPut (CompressedWord.CompressedWord limit dz)

bitGet :: Version.Version -> BitGet.BitGet Vector
bitGet version = BitGet.label "Vector" $ do
  size <-
    BitGet.label "size"
    . CompressedWord.bitGet
    $ if Version.atLeast 868 22 7 version then 21 else 19
  let
    limit = getLimit size
    bias = getBias size
  x <- BitGet.label "x" $ getPart limit bias
  y <- BitGet.label "y" $ getPart limit bias
  z <- BitGet.label "z" $ getPart limit bias
  pure Vector { size, bias, x, y, z }

getPart :: Word -> Word -> BitGet.BitGet Int
getPart limit bias = fmap (fromDelta bias) (CompressedWord.bitGet limit)

getLimit :: CompressedWord.CompressedWord -> Word
getLimit = (2 ^) . (+ 2) . CompressedWord.value

getBias :: CompressedWord.CompressedWord -> Word
getBias = (2 ^) . (+ 1) . CompressedWord.value

fromDelta :: Word -> CompressedWord.CompressedWord -> Int
fromDelta bias_ x_ =
  fromIntegral (CompressedWord.value x_) - fromIntegral bias_
