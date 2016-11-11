module Rattletrap.AttributeValue.LoadoutOnline where

import Rattletrap.Primitive.CompressedWord
import Rattletrap.Word32
import Rattletrap.Primitive.Word8

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype LoadoutOnlineAttributeValue = LoadoutOnlineAttributeValue
  { loadoutAttributeValueValue :: [[(Word32, CompressedWord)]]
  } deriving (Eq, Ord, Show)

getLoadoutOnlineAttributeValue :: BinaryBit.BitGet LoadoutOnlineAttributeValue
getLoadoutOnlineAttributeValue = do
  size <- getWord8Bits
  values <-
    Monad.replicateM
      (fromIntegral (word8Value size))
      (do innerSize <- getWord8Bits
          Monad.replicateM
            (fromIntegral (word8Value innerSize))
            (do x <- getWord32Bits
                y <- getCompressedWord 27
                pure (x, y)))
  pure (LoadoutOnlineAttributeValue values)

putLoadoutOnlineAttributeValue :: LoadoutOnlineAttributeValue
                               -> BinaryBit.BitPut ()
putLoadoutOnlineAttributeValue loadoutAttributeValue = do
  let values = loadoutAttributeValueValue loadoutAttributeValue
  putWord8Bits (Word8 (fromIntegral (length values)))
  mapM_
    (\xs -> do
       putWord8Bits (Word8 (fromIntegral (length xs)))
       mapM_
         (\(x, y) -> do
            putWord32Bits x
            putCompressedWord y)
         xs)
    values
