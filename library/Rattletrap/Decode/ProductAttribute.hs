module Rattletrap.Decode.ProductAttribute
  ( decodeProductAttributesBits
  , decodeProductAttributeBits
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.Common
import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Decode.Str
import Rattletrap.Type.Common
import Rattletrap.Type.CompressedWord
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
  let objectName = Map.lookup objectId objectMap
  value <- case objectName of
    Just name -> case fromStr name of
      "TAGame.ProductAttribute_Painted_TA" -> Just <$> decodePainted version
      "TAGame.ProductAttribute_UserColor_TA" -> decodeColor
      _ -> decodeWhen False (Right <$> getWord32be 31)
    Nothing -> fail ("missing object name for ID " <> show objectId) 
  value2 <- case objectName of
    Just name -> case fromStr name of      
      "TAGame.ProductAttribute_TitleID_TA" -> decodeTitle
      _ -> decodeWhen False (decodeStrBits)
    Nothing -> fail ("missing object name for ID " <> show objectId)  
  pure (ProductAttribute flag objectId objectName value value2)

decodePainted :: (Int, Int, Int) -> DecodeBits (Either CompressedWord Word32)
decodePainted version = if version >= (868, 18, 0)
  then Right <$> getWord32be 31
  else Left <$> decodeCompressedWordBits 13

decodeColor :: DecodeBits (Maybe (Either CompressedWord Word32))
decodeColor = do
  hasValue <- getBool
  decodeWhen hasValue (Right <$> getWord32be 31)
  
decodeTitle :: DecodeBits (Maybe Str)
decodeTitle = do
  decodeWhen True (decodeStrBits)
  