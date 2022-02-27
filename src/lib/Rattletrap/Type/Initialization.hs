module Rattletrap.Type.Initialization where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data Initialization = Initialization
  { location :: Maybe Vector.Vector
  -- ^ Not every class has an initial location. See
  -- 'Rattletrap.Data.classesWithLocation'.
  , rotation :: Maybe Int8Vector.Int8Vector
  -- ^ Only classes with location can have rotation, but not every one does.
  -- See 'Rattletrap.Data.classesWithRotation'.
  }
  deriving (Eq, Show)

instance Argo.HasCodec Initialization where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ Initialization
      <$> Argo.optional location "location"
      <*> Argo.optional rotation "rotation"

bitPut :: Initialization -> BitPut.BitPut
bitPut initialization =
  foldMap Vector.bitPut (location initialization)
    <> foldMap Int8Vector.bitPut (rotation initialization)

bitGet :: Version.Version -> Bool -> Bool -> BitGet.BitGet Initialization
bitGet version hasLocation hasRotation = BitGet.label "Initialization" $ do
  location <- BitGet.label "location"
    $ Monad.whenMaybe hasLocation (Vector.bitGet version)
  rotation <- BitGet.label "rotation"
    $ Monad.whenMaybe hasRotation Int8Vector.bitGet
  pure Initialization { location, rotation }
