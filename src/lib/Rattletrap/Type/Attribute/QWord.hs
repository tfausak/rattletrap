{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.QWord where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word64le as Word64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype QWordAttribute = QWordAttribute
  { qWordAttributeValue :: Word64le.Word64le
  } deriving (Eq, Show)

$(deriveJson ''QWordAttribute)

putQWordAttribute :: QWordAttribute -> BitPut ()
putQWordAttribute qWordAttribute =
  Word64le.bitPut (qWordAttributeValue qWordAttribute)

decodeQWordAttributeBits :: BitGet QWordAttribute
decodeQWordAttributeBits = QWordAttribute <$> Word64le.bitGet
