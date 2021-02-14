{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int64 where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int64le as Int64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype Int64Attribute = Int64Attribute
  { value :: Int64le.Int64le
  } deriving (Eq, Show)

$(deriveJson ''Int64Attribute)

putInt64Attribute :: Int64Attribute -> BitPut ()
putInt64Attribute int64Attribute =
  Int64le.bitPut (value int64Attribute)

bitGet :: BitGet Int64Attribute
bitGet = Int64Attribute <$> Int64le.bitGet
