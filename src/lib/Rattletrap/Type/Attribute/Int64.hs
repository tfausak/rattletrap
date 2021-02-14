{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int64 where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I64 as I64
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype Int64Attribute = Int64Attribute
  { value :: I64.I64
  } deriving (Eq, Show)

$(deriveJson ''Int64Attribute)

putInt64Attribute :: Int64Attribute -> BitPut ()
putInt64Attribute int64Attribute =
  I64.bitPut (value int64Attribute)

bitGet :: BitGet Int64Attribute
bitGet = Int64Attribute <$> I64.bitGet
