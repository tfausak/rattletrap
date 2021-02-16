module Rattletrap.Type.Attribute.StatEvent where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32

data StatEvent = StatEvent
  { unknown :: Bool
  , objectId :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''StatEvent)

bitPut :: StatEvent -> BitPut.BitPut
bitPut statEventAttribute = BitPut.bool (unknown statEventAttribute)
  <> I32.bitPut (objectId statEventAttribute)

bitGet :: BitGet.BitGet StatEvent
bitGet = StatEvent <$> BitGet.bool <*> I32.bitGet
