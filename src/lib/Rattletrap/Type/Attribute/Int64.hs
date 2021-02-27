module Rattletrap.Type.Attribute.Int64 where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common hiding (Int64)
import qualified Rattletrap.Type.I64 as I64

newtype Int64 = Int64
  { value :: I64.I64
  } deriving (Eq, Show)

$(deriveJson ''Int64)

schema :: Schema.Schema
schema = Schema.named "attribute-int64" $ Schema.ref I64.schema

putInt64Attribute :: Int64 -> BitPut.BitPut
putInt64Attribute int64Attribute = I64.bitPut (value int64Attribute)

bitGet :: BitGet.BitGet Int64
bitGet = Int64 <$> I64.bitGet
