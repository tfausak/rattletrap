module Rattletrap.Type.Attribute.WeldedInfo where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.F32 as F32
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version

data WeldedInfo = WeldedInfo
  { active :: Bool
  , actorId :: I32.I32
  , offset :: Vector.Vector
  , mass :: F32.F32
  , rotation :: Int8Vector.Int8Vector
  }
  deriving (Eq, Show)

$(deriveJson ''WeldedInfo)

bitPut :: WeldedInfo -> BitPut.BitPut
bitPut weldedInfoAttribute =
  BitPut.bool (active weldedInfoAttribute)
    <> I32.bitPut (actorId weldedInfoAttribute)
    <> Vector.bitPut (offset weldedInfoAttribute)
    <> F32.bitPut (mass weldedInfoAttribute)
    <> Int8Vector.bitPut (rotation weldedInfoAttribute)

bitGet :: Version.Version -> BitGet.BitGet WeldedInfo
bitGet version =
  WeldedInfo
    <$> BitGet.bool
    <*> I32.bitGet
    <*> Vector.bitGet version
    <*> F32.bitGet
    <*> Int8Vector.bitGet
