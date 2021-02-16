module Rattletrap.Type.Attribute.FlaggedInt where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32

data FlaggedInt = FlaggedInt
  { flag :: Bool
  , int :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedInt)

bitPut :: FlaggedInt -> BitPut.BitPut
bitPut flaggedIntAttribute = BitPut.bool (flag flaggedIntAttribute)
  <> I32.bitPut (int flaggedIntAttribute)

bitGet :: BitGet.BitGet FlaggedInt
bitGet = FlaggedInt <$> BitGet.bool <*> I32.bitGet
