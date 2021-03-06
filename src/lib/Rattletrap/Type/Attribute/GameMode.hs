module Rattletrap.Type.Attribute.GameMode where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data GameMode = GameMode
  { numBits :: Int
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Int' rather than something more precise like an
  -- 'Int8' because it just gets passed to functions that expect 'Int's.
  -- There's no reason to do a bunch of conversions.
  , word :: Word.Word8
  }
  deriving (Eq, Show)

instance Json.FromJSON GameMode where
  parseJSON = Json.withObject "GameMode" $ \object -> do
    numBits <- Json.required object "num_bits"
    word <- Json.required object "word"
    pure GameMode { numBits, word }

instance Json.ToJSON GameMode where
  toJSON x =
    Json.object [Json.pair "num_bits" $ numBits x, Json.pair "word" $ word x]

schema :: Schema.Schema
schema = Schema.named "attribute-game-mode" $ Schema.object
  [ (Json.pair "num_bits" $ Schema.ref Schema.integer, True)
  , (Json.pair "word" $ Schema.ref Schema.integer, True)
  ]

bitPut :: GameMode -> BitPut.BitPut
bitPut gameModeAttribute = do
  BitPut.word8 (numBits gameModeAttribute) (word gameModeAttribute)

bitGet :: Version.Version -> BitGet.BitGet GameMode
bitGet version = do
  let numBits = if Version.atLeast 868 12 0 version then 8 else 2 :: Int
  word <- BitGet.word8 numBits
  pure GameMode { numBits, word }
