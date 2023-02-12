module Rattletrap.Type.Attribute.Product where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.ProductValue as ProductValue
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Product = Product
  { unknown :: Bool,
    objectId :: U32.U32,
    -- | read-only
    objectName :: Maybe Str.Str,
    value :: ProductValue.ProductValue
  }
  deriving (Eq, Show)

instance Json.FromJSON Product where
  parseJSON = Json.withObject "Product" $ \object -> do
    unknown <- Json.required object "unknown"
    objectId <- Json.required object "object_id"
    objectName <- Json.optional object "object_name"
    value <- Json.required object "value"
    pure Product {unknown, objectId, objectName, value}

instance Json.ToJSON Product where
  toJSON x =
    Json.object
      [ Json.pair "unknown" $ unknown x,
        Json.pair "object_id" $ objectId x,
        Json.pair "object_name" $ objectName x,
        Json.pair "value" $ value x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute-product" $
    Schema.object
      [ (Json.pair "unknown" $ Schema.ref Schema.boolean, True),
        (Json.pair "object_id" $ Schema.ref U32.schema, True),
        (Json.pair "object_name" . Schema.json $ Schema.maybe Str.schema, False),
        (Json.pair "value" $ Schema.ref ProductValue.schema, True)
      ]

putProductAttributes :: List.List Product -> BitPut.BitPut
putProductAttributes attributes =
  let v = List.toList attributes
   in (U8.bitPut . U8.fromWord8 . fromIntegral $ length v) <> foldMap bitPut v

bitPut :: Product -> BitPut.BitPut
bitPut attribute =
  BitPut.bool (unknown attribute)
    <> U32.bitPut (objectId attribute)
    <> ProductValue.bitPut (value attribute)

decodeProductAttributesBits ::
  Version.Version ->
  Map.Map U32.U32 Str.Str ->
  BitGet.BitGet (List.List Product)
decodeProductAttributesBits version objectMap = do
  size <- U8.bitGet
  List.replicateM (fromIntegral $ U8.toWord8 size) $ bitGet version objectMap

bitGet :: Version.Version -> Map.Map U32.U32 Str.Str -> BitGet.BitGet Product
bitGet version objectMap = BitGet.label "Product" $ do
  unknown <- BitGet.label "unknown" BitGet.bool
  objectId <- BitGet.label "objectId" U32.bitGet
  let objectName = Map.lookup objectId objectMap
  value <-
    BitGet.label "value" $
      ProductValue.bitGet version objectId objectName
  pure Product {unknown, objectId, objectName, value}
