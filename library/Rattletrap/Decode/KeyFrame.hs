module Rattletrap.Decode.KeyFrame
  ( decodeKeyFrame
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Float32le
import Rattletrap.Decode.Word32le
import Rattletrap.Type.KeyFrame

decodeKeyFrame :: Decode KeyFrame
decodeKeyFrame =
  KeyFrame <$> decodeFloat32le <*> decodeWord32le <*> decodeWord32le
