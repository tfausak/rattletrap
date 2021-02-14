{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.QWord where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word64le as Word64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype QWordAttribute = QWordAttribute
  { value :: Word64le.Word64le
  } deriving (Eq, Show)

$(deriveJson ''QWordAttribute)

bitPut :: QWordAttribute -> BitPut ()
bitPut qWordAttribute =
  Word64le.bitPut (value qWordAttribute)

bitGet :: BitGet QWordAttribute
bitGet = QWordAttribute <$> Word64le.bitGet
