module Rattletrap.Decode.KeyFrame
  ( getKeyFrame
  ) where

import Rattletrap.Type.KeyFrame
import Rattletrap.Decode.Float32
import Rattletrap.Decode.Word32

import qualified Data.Binary as Binary

getKeyFrame :: Binary.Get KeyFrame
getKeyFrame = do
  time <- getFloat32
  frame <- getWord32
  position <- getWord32
  pure (KeyFrame time frame position)