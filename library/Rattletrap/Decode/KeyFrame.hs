module Rattletrap.Decode.KeyFrame
  ( getKeyFrame
  ) where

import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Word32le
import Rattletrap.Type.KeyFrame

import qualified Data.Binary as Binary

getKeyFrame :: Binary.Get KeyFrame
getKeyFrame = do
  time <- getFloat32
  frame <- getWord32
  position <- getWord32
  pure (KeyFrame time frame position)
