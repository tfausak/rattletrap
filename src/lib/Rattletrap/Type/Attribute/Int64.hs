{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int64 where

import Rattletrap.Type.Common hiding (Int64)
import qualified Rattletrap.Type.I64 as I64
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

newtype Int64 = Int64
  { value :: I64.I64
  } deriving (Eq, Show)

$(deriveJson ''Int64)

putInt64Attribute :: Int64 -> BitPut.BitPut
putInt64Attribute int64Attribute =
  I64.bitPut (value int64Attribute)

bitGet :: BitGet.BitGet Int64
bitGet = Int64 <$> I64.bitGet
