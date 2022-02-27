module Rattletrap.Type.Int8Vector where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I8 as I8
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data Int8Vector = Int8Vector
  { x :: Maybe I8.I8
  , y :: Maybe I8.I8
  , z :: Maybe I8.I8
  }
  deriving (Eq, Show)

instance Argo.HasCodec Int8Vector where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ Int8Vector
      <$> Argo.optional x "x"
      <*> Argo.optional y "y"
      <*> Argo.optional z "z"

bitPut :: Int8Vector -> BitPut.BitPut
bitPut int8Vector =
  putInt8VectorField (x int8Vector)
    <> putInt8VectorField (y int8Vector)
    <> putInt8VectorField (z int8Vector)

putInt8VectorField :: Maybe I8.I8 -> BitPut.BitPut
putInt8VectorField maybeField = case maybeField of
  Nothing -> BitPut.bool False
  Just field -> BitPut.bool True <> I8.bitPut field

bitGet :: BitGet.BitGet Int8Vector
bitGet = BitGet.label "Int8Vector" $ do
  x <- BitGet.label "x" decodeFieldBits
  y <- BitGet.label "y" decodeFieldBits
  z <- BitGet.label "z" decodeFieldBits
  pure Int8Vector { x, y, z }

decodeFieldBits :: BitGet.BitGet (Maybe I8.I8)
decodeFieldBits = do
  hasField <- BitGet.bool
  Monad.whenMaybe hasField I8.bitGet
