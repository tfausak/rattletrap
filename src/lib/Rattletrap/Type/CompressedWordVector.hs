module Rattletrap.Type.CompressedWordVector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord

data CompressedWordVector = CompressedWordVector
  { x :: CompressedWord.CompressedWord
  , y :: CompressedWord.CompressedWord
  , z :: CompressedWord.CompressedWord
  }
  deriving (Eq, Show)

$(deriveJson ''CompressedWordVector)

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
