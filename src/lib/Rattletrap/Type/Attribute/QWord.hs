{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.QWord where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype QWordAttribute = QWordAttribute
  { qWordAttributeValue :: Word64le
  } deriving (Eq, Show)

$(deriveJson ''QWordAttribute)

putQWordAttribute :: QWordAttribute -> BitPut ()
putQWordAttribute qWordAttribute =
  putWord64Bits (qWordAttributeValue qWordAttribute)

decodeQWordAttributeBits :: BitGet QWordAttribute
decodeQWordAttributeBits = QWordAttribute <$> decodeWord64leBits
