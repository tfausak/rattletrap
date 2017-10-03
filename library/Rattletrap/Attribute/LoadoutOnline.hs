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
  , productAttributeValue :: Maybe (Either CompressedWord Word.Word32)
  } deriving (Eq, Ord, Show)

getLoadoutOnlineAttribute ::
     (Int, Int)
  -> Map.Map Word32 Text
  -> BinaryBit.BitGet LoadoutOnlineAttribute
getLoadoutOnlineAttribute version objectMap = do
  size <- getWord8Bits
  values <-
    Monad.replicateM
      (fromIntegral (word8Value size))
      (getProductAttributes version objectMap)
  pure (LoadoutOnlineAttribute values)

getProductAttributes ::
     (Int, Int) -> Map.Map Word32 Text -> BinaryBit.BitGet [ProductAttribute]
getProductAttributes version objectMap = do
  size <- getWord8Bits
  Monad.replicateM
    (fromIntegral (word8Value size))
    (getProductAttribute version objectMap)

getProductAttribute ::
     (Int, Int) -> Map.Map Word32 Text -> BinaryBit.BitGet ProductAttribute
getProductAttribute version objectMap = do
  flag <- BinaryBit.getBool
  objectId <- getWord32Bits
  let objectName = Map.lookup objectId objectMap
  value <-
    case objectName of
      Just name ->
        case textToString name of
          "TAGame.ProductAttribute_Painted_TA" ->
            if version >= (868, 18)
              then do
                x <- BinaryBit.getWord32be 31
                pure (Just (Right x))
              else do
                x <- getCompressedWord 14
                pure (Just (Left x))
          "TAGame.ProductAttribute_UserColor_TA" -> do
            hasValue <- BinaryBit.getBool
            value <-
              if hasValue
                then do
                  x <- BinaryBit.getWord32be 31
                  pure (Just (Right x))
                else pure Nothing
            pure value
          _ ->
            fail
              ("unknown object name " ++
               show objectName ++ " for ID " ++ show objectId)
      Nothing -> fail ("missing object name for ID " ++ show objectId)
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
  case productAttributeObjectName attribute of
    Just name ->
      case textToString name of
        "TAGame.ProductAttribute_Painted_TA" ->
          case productAttributeValue attribute of
            Nothing -> pure ()
            Just (Left x) -> putCompressedWord x
            Just (Right x) -> BinaryBit.putWord32be 31 x
        "TAGame.ProductAttribute_UserColor_TA" ->
          case productAttributeValue attribute of
            Nothing -> BinaryBit.putBool False
            Just value -> do
              BinaryBit.putBool True
              case value of
                Left x -> putCompressedWord x
                Right x -> BinaryBit.putWord32be 31 x
        _ ->
          fail ("unknown object name for product attribute " ++ show attribute)
    Nothing ->
      fail ("missing object name for product attribute " ++ show attribute)
