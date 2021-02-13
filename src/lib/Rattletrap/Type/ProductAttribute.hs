{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttribute where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Encode.Common
import Rattletrap.Type.Word8le
import Rattletrap.Decode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttributeValue
  = ProductAttributeValuePaintedOld CompressedWord
  | ProductAttributeValuePaintedNew Word32
  | ProductAttributeValueTeamEditionOld CompressedWord
  | ProductAttributeValueTeamEditionNew Word32
  | ProductAttributeValueSpecialEdition Word32
  | ProductAttributeValueUserColorOld (Maybe Word32)
  | ProductAttributeValueUserColorNew Word32le
  | ProductAttributeValueTitleId Str
  deriving (Eq, Show)

$(deriveJson ''ProductAttributeValue)

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32le
  , productAttributeObjectName :: Maybe Str
  -- ^ read-only
  , productAttributeValue :: ProductAttributeValue
  }
  deriving (Eq, Show)

$(deriveJson ''ProductAttribute)

putProductAttributes :: [ProductAttribute] -> BinaryBits.BitPut ()
putProductAttributes attributes = do
  putWord8Bits (Word8le (fromIntegral (length attributes)))
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BinaryBits.BitPut ()
putProductAttribute attribute = do
  BinaryBits.putBool (productAttributeUnknown attribute)
  putWord32Bits (productAttributeObjectId attribute)
  case productAttributeValue attribute of
    ProductAttributeValuePaintedOld x -> putCompressedWord x
    ProductAttributeValuePaintedNew x -> putBitsLE 31 x
    ProductAttributeValueTeamEditionOld x -> putCompressedWord x
    ProductAttributeValueTeamEditionNew x -> putBitsLE 31 x
    ProductAttributeValueSpecialEdition x -> putBitsLE 31 x
    ProductAttributeValueUserColorOld x -> case x of
      Nothing -> BinaryBits.putBool False
      Just y -> do
        BinaryBits.putBool True
        putBitsLE 31 y
    ProductAttributeValueUserColorNew x -> putWord32Bits x
    ProductAttributeValueTitleId x -> putTextBits x

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
