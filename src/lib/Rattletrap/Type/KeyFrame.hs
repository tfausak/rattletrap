{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.KeyFrame where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data KeyFrame = KeyFrame
  { time :: Float32le.Float32le
  -- ^ When this key frame occurs, in seconds.
  , frame :: Word32le.Word32le
  -- ^ The frame number of this key frame, starting from 0.
  , position :: Word32le.Word32le
  -- ^ The bit position of this key frame in the stream.
  }
  deriving (Eq, Show)

$(deriveJson ''KeyFrame)

bytePut :: KeyFrame -> BytePut
bytePut keyFrame = do
  Float32le.bytePut (time keyFrame)
  Word32le.bytePut (frame keyFrame)
  Word32le.bytePut (position keyFrame)

byteGet :: ByteGet KeyFrame
byteGet =
  KeyFrame <$> Float32le.byteGet <*> Word32le.byteGet <*> Word32le.byteGet
