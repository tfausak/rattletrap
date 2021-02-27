module Rattletrap.Type.CompressedWordVector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Utility.Json as Json

data CompressedWordVector = CompressedWordVector
  { x :: CompressedWord.CompressedWord
  , y :: CompressedWord.CompressedWord
  , z :: CompressedWord.CompressedWord
  }
  deriving (Eq, Show)

$(deriveJson ''CompressedWordVector)

schema :: Schema.Schema
schema = Schema.named "compressed-word-vector" $ Schema.object
  [ (Json.pair "x" $ Schema.ref CompressedWord.schema, True)
  , (Json.pair "y" $ Schema.ref CompressedWord.schema, True)
  , (Json.pair "z" $ Schema.ref CompressedWord.schema, True)
  ]

bitPut :: CompressedWordVector -> BitPut.BitPut
bitPut compressedWordVector =
  CompressedWord.bitPut (x compressedWordVector)
    <> CompressedWord.bitPut (y compressedWordVector)
    <> CompressedWord.bitPut (z compressedWordVector)

bitGet :: BitGet.BitGet CompressedWordVector
bitGet =
  CompressedWordVector
    <$> CompressedWord.bitGet limit
    <*> CompressedWord.bitGet limit
    <*> CompressedWord.bitGet limit

limit :: Word
limit = 65536
