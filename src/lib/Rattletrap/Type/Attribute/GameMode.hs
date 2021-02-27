module Rattletrap.Type.Attribute.GameMode where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data GameMode = GameMode
  { numBits :: Int
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Int' rather than something more precise like an
  -- 'Int8' because it just gets passed to functions that expect 'Int's.
  -- There's no reason to do a bunch of conversions.
  , word :: Word8
  }
  deriving (Eq, Show)

$(deriveJson ''GameMode)

schema :: Schema.Schema
schema = Schema.named "attribute-game-mode" $ Schema.object
  [ (Json.pair "num_bits" $ Schema.ref Schema.integer, True)
  , (Json.pair "word" $ Schema.ref Schema.integer, True)
  ]

bitPut :: GameMode -> BitPut.BitPut
bitPut gameModeAttribute = do
  BitPut.word8 (numBits gameModeAttribute) (word gameModeAttribute)

bitGet :: Version.Version -> BitGet.BitGet GameMode
bitGet version =
  GameMode (numBits_ version) <$> BitGet.word8 (numBits_ version)

numBits_ :: Version.Version -> Int
numBits_ version = if has8Bits version then 8 else 2

has8Bits :: Version.Version -> Bool
has8Bits v =
  Version.major v >= 868 && Version.minor v >= 12 && Version.patch v >= 0
