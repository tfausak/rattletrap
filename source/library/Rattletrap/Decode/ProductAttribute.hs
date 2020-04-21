module Rattletrap.Decode.ProductAttribute
  ( decodeProductAttributesBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Decode.Str
import Rattletrap.Type.Common
import Rattletrap.Type.ProductAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

import qualified Control.Monad as Monad
import qualified Data.Map as Map

decodeProductAttributesBits
  :: (Int, Int, Int) -> Map Word32le Str -> DecodeBits [ProductAttribute]
decodeProductAttributesBits version objectMap = do
  size <- decodeWord8leBits
  Monad.replicateM
    (fromIntegral (word8leValue size))
    (decodeProductAttributeBits version objectMap)

decodeProductAttributeBits
  :: (Int, Int, Int) -> Map Word32le Str -> DecodeBits ProductAttribute
decodeProductAttributeBits version objectMap = do
  flag <- getBool
  objectId <- decodeWord32leBits
  let maybeObjectName = Map.lookup objectId objectMap
  value <- case fromStr <$> maybeObjectName of
    Just "TAGame.ProductAttribute_Painted_TA" -> decodePainted version
    Just "TAGame.ProductAttribute_SpecialEdition_TA" -> decodeSpecialEdition
    Just "TAGame.ProductAttribute_TeamEdition_TA" -> decodeTeamEdition version
    Just "TAGame.ProductAttribute_TitleID_TA" -> decodeTitle
    Just "TAGame.ProductAttribute_UserColor_TA" -> decodeColor version
    Just objectName -> fail
      ("[RT05] unknown object name "
      <> show objectName
      <> " for ID "
      <> show objectId
      )
    Nothing -> fail ("[RT06] missing object name for ID " <> show objectId)
  pure (ProductAttribute flag objectId maybeObjectName value)

decodeSpecialEdition :: DecodeBits ProductAttributeValue
decodeSpecialEdition = ProductAttributeValueSpecialEdition <$> getBitsLE 31

decodePainted :: (Int, Int, Int) -> DecodeBits ProductAttributeValue
decodePainted version = if version >= (868, 18, 0)
  then ProductAttributeValuePaintedNew <$> getBitsLE 31
  else ProductAttributeValuePaintedOld <$> decodeCompressedWordBits 13

decodeTeamEdition :: (Int, Int, Int) -> DecodeBits ProductAttributeValue
decodeTeamEdition version = if version >= (868, 18, 0)
  then ProductAttributeValueTeamEditionNew <$> getBitsLE 31
  else ProductAttributeValueTeamEditionOld <$> decodeCompressedWordBits 13

decodeColor :: (Int, Int, Int) -> DecodeBits ProductAttributeValue
decodeColor version = if version >= (868, 23, 8)
  then ProductAttributeValueUserColorNew <$> decodeWord32leBits
  else do
    hasValue <- getBool
    ProductAttributeValueUserColorOld <$> decodeWhen hasValue (getBitsLE 31)

decodeTitle :: DecodeBits ProductAttributeValue
decodeTitle = ProductAttributeValueTitleId <$> decodeStrBits
