module Rattletrap.Decode.ProductAttribute
  ( getProductAttributes
  , getProductAttribute
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.CompressedWord
import Rattletrap.Decode.Word32le
import Rattletrap.Decode.Word8le
import Rattletrap.Type.ProductAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Map as Map

getProductAttributes
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> BinaryBit.BitGet [ProductAttribute]
getProductAttributes version objectMap = do
  size <- getWord8Bits
  Monad.replicateM
    (fromIntegral (word8leValue size))
    (getProductAttribute version objectMap)

getProductAttribute
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> BinaryBit.BitGet ProductAttribute
getProductAttribute version objectMap = do
  flag <- BinaryBit.getBool
  objectId <- getWord32Bits
  let objectName = Map.lookup objectId objectMap
  value <- case objectName of
    Just name -> case fromStr name of
      "TAGame.ProductAttribute_Painted_TA" -> if version >= (868, 18, 0)
        then do
          x <- BinaryBit.getWord32be 31
          pure (Just (Right x))
        else do
          x <- decodeCompressedWordBits 13
          pure (Just (Left x))
      "TAGame.ProductAttribute_UserColor_TA" -> do
        hasValue <- BinaryBit.getBool
        value <- if hasValue
          then do
            x <- BinaryBit.getWord32be 31
            pure (Just (Right x))
          else pure Nothing
        pure value
      _ ->
        fail
          ( "unknown object name "
          <> show objectName
          <> " for ID "
          <> show objectId
          )
    Nothing -> fail ("missing object name for ID " <> show objectId)
  pure (ProductAttribute flag objectId objectName value)
