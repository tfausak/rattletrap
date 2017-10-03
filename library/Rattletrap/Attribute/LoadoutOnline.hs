module Rattletrap.Attribute.LoadoutOnline where

import Rattletrap.Primitive

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map
import qualified Data.Word as Word

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Ord, Show)

data ProductAttribute = ProductAttribute
  { productAttributeUnknown :: Bool
  , productAttributeObjectId :: Word32
  , productAttributeObjectName :: Maybe Text
  -- ^ read-only
  , productAttributeValue :: Maybe Word.Word32
  } deriving (Eq, Ord, Show)

getLoadoutOnlineAttribute ::
     (Int, Int) -> Map.Map Word32 Text -> BinaryBit.BitGet LoadoutOnlineAttribute
getLoadoutOnlineAttribute version objectMap = do
  size <- getWord8Bits
  values <- mapM (getProductAttributes objectMap) [1 .. word8Value size]
  pure (LoadoutOnlineAttribute values)

getProductAttributes :: Map.Map Word32 Text -> Word.Word8 -> BinaryBit.BitGet [ProductAttribute]
getProductAttributes objectMap i = do
  size <- getWord8Bits
  mapM (getProductAttribute objectMap) [1 .. word8Value size]

getProductAttribute :: Map.Map Word32 Text -> Word.Word8 -> BinaryBit.BitGet ProductAttribute
getProductAttribute objectMap i = do
  flag <- BinaryBit.getBool
  objectId <- getWord32Bits
  let objectName = Map.lookup objectId objectMap
  value <- case fmap textToString objectName of
    Just "TAGame.ProductAttribute_UserColor_TA" -> do
      hasValue <- BinaryBit.getBool
      value <- if hasValue then fmap Just (BinaryBit.getWord32be 31) else pure Nothing
      pure value
    _ -> fail ("unknown object name " ++ show objectName ++ " for ID " ++ show objectId)
  pure (ProductAttribute flag objectId objectName value)

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBit.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let attributes = loadoutAttributeValue loadoutAttribute
  putWord8Bits (Word8 (fromIntegral (length attributes)))
  mapM_ putProductAttributes attributes

putProductAttributes :: [ProductAttribute] -> BinaryBit.BitPut ()
putProductAttributes attributes = do
  putWord8Bits (Word8 (fromIntegral (length attributes)))
  mapM_ putProductAttribute attributes

putProductAttribute :: ProductAttribute -> BinaryBit.BitPut ()
putProductAttribute attribute = do
  BinaryBit.putBool (productAttributeUnknown attribute)
  putWord32Bits (productAttributeObjectId attribute)
  case fmap textToString (productAttributeObjectName attribute) of
    Just "TAGame.ProductAttribute_UserColor_TA" -> case productAttributeValue attribute of
      Nothing -> BinaryBit.putBool False
      Just value -> do
        BinaryBit.putBool True
        BinaryBit.putWord32be 31 value
    _ -> fail ("bad product attribute: " ++ show attribute)
