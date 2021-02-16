module Rattletrap.Type.Rotation where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWordVector as CompressedWordVector
import qualified Rattletrap.Type.Quaternion as Quaternion

data Rotation
  = CompressedWordVector CompressedWordVector.CompressedWordVector
  | Quaternion Quaternion.Quaternion
  deriving (Eq, Show)

$(deriveJson ''Rotation)

bitPut :: Rotation -> BitPut.BitPut
bitPut r = case r of
  CompressedWordVector cwv -> CompressedWordVector.bitPut cwv
  Quaternion q -> Quaternion.bitPut q

bitGet :: (Int, Int, Int) -> BitGet.BitGet Rotation
bitGet version = if version >= (868, 22, 7)
  then Quaternion <$> Quaternion.bitGet
  else CompressedWordVector <$> CompressedWordVector.bitGet
