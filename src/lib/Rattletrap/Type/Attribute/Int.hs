{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype IntAttribute = IntAttribute
  { intAttributeValue :: Int32le.Int32le
  } deriving (Eq, Show)

$(deriveJson ''IntAttribute)

putIntAttribute :: IntAttribute -> BitPut ()
putIntAttribute intAttribute = Int32le.bitPut (intAttributeValue intAttribute)

decodeIntAttributeBits :: BitGet IntAttribute
decodeIntAttributeBits = IntAttribute <$> Int32le.bitGet
