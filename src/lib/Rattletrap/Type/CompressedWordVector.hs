module Rattletrap.Type.CompressedWordVector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Utility.Json as Json

data CompressedWordVector = CompressedWordVector
  { x :: CompressedWord.CompressedWord
  , y :: CompressedWord.CompressedWord
  , z :: CompressedWord.CompressedWord
  }
  deriving (Eq, Show)

instance Json.FromJSON CompressedWordVector where
  parseJSON = Json.withObject "CompressedWordVector" $ \object -> do
    x <- Json.required object "x"
    y <- Json.required object "y"
    z <- Json.required object "z"
    pure CompressedWordVector { x, y, z }

instance Json.ToJSON CompressedWordVector where
  toJSON a =
    Json.object [Json.pair "x" $ x a, Json.pair "y" $ y a, Json.pair "z" $ z a]

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
bitGet = do
  x <- CompressedWord.bitGet limit
  y <- CompressedWord.bitGet limit
  z <- CompressedWord.bitGet limit
  pure CompressedWordVector { x, y, z}

limit :: Word
limit = 65536
