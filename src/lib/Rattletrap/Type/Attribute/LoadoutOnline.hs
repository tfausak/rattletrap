module Rattletrap.Type.Attribute.LoadoutOnline where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.Product as Product
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

newtype LoadoutOnline = LoadoutOnline
  { value :: List.List (List.List Product.Product)
  } deriving (Eq, Show)

instance Argo.HasCodec LoadoutOnline where
  codec = Argo.map LoadoutOnline value Argo.codec

bitPut :: LoadoutOnline -> BitPut.BitPut
bitPut loadoutAttribute =
  let attributes = List.toList $ value loadoutAttribute
  in
    (U8.bitPut . U8.fromWord8 . fromIntegral $ length attributes)
      <> foldMap Product.putProductAttributes attributes

bitGet
  :: Version.Version -> Map.Map U32.U32 Str.Str -> BitGet.BitGet LoadoutOnline
bitGet version objectMap = BitGet.label "LoadoutOnline" $ do
  size <- BitGet.label "size" U8.bitGet
  value <-
    BitGet.label "value"
    . List.replicateM (fromIntegral $ U8.toWord8 size)
    $ Product.decodeProductAttributesBits version objectMap
  pure LoadoutOnline { value }
