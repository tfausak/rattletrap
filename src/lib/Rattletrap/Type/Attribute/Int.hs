module Rattletrap.Type.Attribute.Int where

import Prelude hiding (Int)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32

newtype Int = Int
  { value :: I32.I32
  } deriving (Eq, Show)

$(deriveJson ''Int)

schema :: Schema.Schema
schema = Schema.named "attribute-int" $ Schema.ref I32.schema

bitPut :: Int -> BitPut.BitPut
bitPut intAttribute = I32.bitPut (value intAttribute)

bitGet :: BitGet.BitGet Int
bitGet = Int <$> I32.bitGet
