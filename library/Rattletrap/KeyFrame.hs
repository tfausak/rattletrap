{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.KeyFrame where

import Rattletrap.Float32
import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data KeyFrame = KeyFrame
  { keyFrameTime :: Float32
  , keyFrameFrame :: Word32
  , keyFramePosition :: Word32
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON KeyFrame

instance Aeson.ToJSON KeyFrame

getKeyFrame :: Binary.Get KeyFrame
getKeyFrame = do
  time <- getFloat32
  frame <- getWord32
  position <- getWord32
  pure
    KeyFrame
    {keyFrameTime = time, keyFrameFrame = frame, keyFramePosition = position}

putKeyFrame :: KeyFrame -> Binary.Put
putKeyFrame keyFrame = do
  putFloat32 (keyFrameTime keyFrame)
  putWord32 (keyFrameFrame keyFrame)
  putWord32 (keyFramePosition keyFrame)
