{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype IntAttribute = IntAttribute
  { value :: Int32le.Int32le
  } deriving (Eq, Show)

$(deriveJson ''IntAttribute)

bitPut :: IntAttribute -> BitPut ()
bitPut intAttribute = Int32le.bitPut (value intAttribute)

bitGet :: BitGet IntAttribute
bitGet = IntAttribute <$> Int32le.bitGet
