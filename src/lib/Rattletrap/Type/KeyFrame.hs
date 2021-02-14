{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.KeyFrame where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import qualified Rattletrap.BytePut as BytePut

data KeyFrame = KeyFrame
  { time :: F32.F32
  -- ^ When this key frame occurs, in seconds.
  , frame :: U32.U32
  -- ^ The frame number of this key frame, starting from 0.
  , position :: U32.U32
  -- ^ The bit position of this key frame in the stream.
  }
  deriving (Eq, Show)

$(deriveJson ''KeyFrame)

bytePut :: KeyFrame -> BytePut.BytePut
bytePut keyFrame = do
  F32.bytePut (time keyFrame)
  U32.bytePut (frame keyFrame)
  U32.bytePut (position keyFrame)

byteGet :: ByteGet KeyFrame
byteGet =
  KeyFrame <$> F32.byteGet <*> U32.byteGet <*> U32.byteGet
