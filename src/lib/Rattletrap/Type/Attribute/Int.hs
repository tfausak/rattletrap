{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype IntAttribute = IntAttribute
  { value :: I32.I32
  } deriving (Eq, Show)

$(deriveJson ''IntAttribute)

bitPut :: IntAttribute -> BitPut ()
bitPut intAttribute = I32.bitPut (value intAttribute)

bitGet :: BitGet IntAttribute
bitGet = IntAttribute <$> I32.bitGet
