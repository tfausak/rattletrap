{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Rotation where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWordVector
import Rattletrap.Type.Quaternion
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data Rotation
  = RotationCompressedWordVector CompressedWordVector
  | RotationQuaternion Quaternion
  deriving (Eq, Show)

$(deriveJson ''Rotation)

putRotation :: Rotation -> BinaryBits.BitPut ()
putRotation r = case r of
  RotationCompressedWordVector cwv -> putCompressedWordVector cwv
  RotationQuaternion q -> putQuaternion q

decodeRotationBits :: (Int, Int, Int) -> BitGet Rotation
decodeRotationBits version = if version >= (868, 22, 7)
  then RotationQuaternion <$> decodeQuaternionBits
  else RotationCompressedWordVector <$> decodeCompressedWordVectorBits
