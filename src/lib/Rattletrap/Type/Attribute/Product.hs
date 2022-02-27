module Rattletrap.Type.Attribute.Product where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute.ProductValue as ProductValue
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Product = Product
  { unknown :: Bool
  , objectId :: U32.U32
  , objectName :: Maybe Str.Str
  -- ^ read-only
  , value :: ProductValue.ProductValue
  }
  deriving (Eq, Show)

instance Argo.HasCodec Product where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Product
      <$> Argo.required unknown "unknown"
      <*> Argo.required objectId "object_id"
      <*> Argo.optional objectName "object_name"
      <*> Argo.required value "value"

putProductAttributes :: List.List Product -> BitPut.BitPut
putProductAttributes attributes =
  let v = List.toList attributes
  in (U8.bitPut . U8.fromWord8 . fromIntegral $ length v) <> foldMap bitPut v

bitPut :: Product -> BitPut.BitPut
bitPut attribute =
  BitPut.bool (unknown attribute)
    <> U32.bitPut (objectId attribute)
    <> ProductValue.bitPut (value attribute)

decodeProductAttributesBits
  :: Version.Version
  -> Map.Map U32.U32 Str.Str
  -> BitGet.BitGet (List.List Product)
decodeProductAttributesBits version objectMap = do
  size <- U8.bitGet
  List.replicateM (fromIntegral $ U8.toWord8 size) $ bitGet version objectMap

bitGet :: Version.Version -> Map.Map U32.U32 Str.Str -> BitGet.BitGet Product
bitGet version objectMap = BitGet.label "Product" $ do
  unknown <- BitGet.label "unknown" BitGet.bool
  objectId <- BitGet.label "objectId" U32.bitGet
  let objectName = Map.lookup objectId objectMap
  value <- BitGet.label "value"
    $ ProductValue.bitGet version objectId objectName
  pure Product { unknown, objectId, objectName, value }
