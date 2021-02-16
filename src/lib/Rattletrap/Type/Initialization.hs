module Rattletrap.Type.Initialization where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Utility.Monad

data Initialization = Initialization
  { location :: Maybe Vector.Vector
  -- ^ Not every class has an initial location. See
  -- 'Rattletrap.Data.classesWithLocation'.
  , rotation :: Maybe Int8Vector.Int8Vector
  -- ^ Only classes with location can have rotation, but not every one does.
  -- See 'Rattletrap.Data.classesWithRotation'.
  }
  deriving (Eq, Show)

$(deriveJson ''Initialization)

bitPut :: Initialization -> BitPut.BitPut
bitPut initialization =
  foldMap Vector.bitPut (location initialization)
    <> foldMap Int8Vector.bitPut (rotation initialization)

bitGet :: (Int, Int, Int) -> Bool -> Bool -> BitGet.BitGet Initialization
bitGet version hasLocation hasRotation =
  Initialization
    <$> whenMaybe hasLocation (Vector.bitGet version)
    <*> whenMaybe hasRotation Int8Vector.bitGet
