module Rattletrap.Decode.Mark
  ( decodeMark
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Str
import Rattletrap.Decode.Word32le
import Rattletrap.Type.Mark

decodeMark :: Decode Mark
decodeMark = Mark <$> decodeStr <*> decodeWord32le
