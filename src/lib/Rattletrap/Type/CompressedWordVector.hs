module Rattletrap.Type.CompressedWordVector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Vendor.Argo as Argo

data CompressedWordVector = CompressedWordVector
  { x :: CompressedWord.CompressedWord
  , y :: CompressedWord.CompressedWord
  , z :: CompressedWord.CompressedWord
  }
  deriving (Eq, Show)

instance Argo.HasCodec CompressedWordVector where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ CompressedWordVector
      <$> Argo.project x (Argo.required "x" Argo.codec)
      <*> Argo.project y (Argo.required "y" Argo.codec)
      <*> Argo.project z (Argo.required "z" Argo.codec)

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
  pure CompressedWordVector { x, y, z }

limit :: Word
limit = 65536
