{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ProductAttributeValue where

import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data ProductAttributeValue
  = ProductAttributeValuePaintedOld CompressedWord
  | ProductAttributeValuePaintedNew Word32
  | ProductAttributeValueTeamEditionOld CompressedWord
  | ProductAttributeValueTeamEditionNew Word32
  | ProductAttributeValueSpecialEdition Word32
  | ProductAttributeValueUserColorOld (Maybe Word32)
  | ProductAttributeValueUserColorNew Word32le.Word32le
  | ProductAttributeValueTitleId Str.Str
  deriving (Eq, Show)

$(deriveJson ''ProductAttributeValue)

putProductAttributeValue :: ProductAttributeValue -> BitPut ()
putProductAttributeValue val = case val of
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
  ProductAttributeValueUserColorNew x -> Word32le.bitPut x
  ProductAttributeValueTitleId x -> Str.bitPut x

decodeProductAttributeValueBits
  :: (Int, Int, Int) -> Word32le.Word32le -> Maybe Str.Str -> BitGet ProductAttributeValue
decodeProductAttributeValueBits version objectId maybeObjectName =
  case Str.toString <$> maybeObjectName of
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

decodeSpecialEdition :: BitGet ProductAttributeValue
decodeSpecialEdition = ProductAttributeValueSpecialEdition <$> getBitsLE 31

decodePainted :: (Int, Int, Int) -> BitGet ProductAttributeValue
decodePainted version = if version >= (868, 18, 0)
  then ProductAttributeValuePaintedNew <$> getBitsLE 31
  else ProductAttributeValuePaintedOld <$> decodeCompressedWordBits 13

decodeTeamEdition :: (Int, Int, Int) -> BitGet ProductAttributeValue
decodeTeamEdition version = if version >= (868, 18, 0)
  then ProductAttributeValueTeamEditionNew <$> getBitsLE 31
  else ProductAttributeValueTeamEditionOld <$> decodeCompressedWordBits 13

decodeColor :: (Int, Int, Int) -> BitGet ProductAttributeValue
decodeColor version = if version >= (868, 23, 8)
  then ProductAttributeValueUserColorNew <$> Word32le.bitGet
  else do
    hasValue <- getBool
    ProductAttributeValueUserColorOld <$> decodeWhen hasValue (getBitsLE 31)

decodeTitle :: BitGet ProductAttributeValue
decodeTitle = ProductAttributeValueTitleId <$> Str.bitGet
