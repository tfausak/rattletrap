module Rattletrap.Attribute.LoadoutOnline where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Vector as Vector

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: Vector.Vector (Vector.Vector (Word32, CompressedWord))
  } deriving (Eq, Show)

getLoadoutOnlineAttribute :: BinaryBit.BitGet LoadoutOnlineAttribute
getLoadoutOnlineAttribute = do
  size <- getWord8Bits
  values <-
    Vector.replicateM
      (fromIntegral (word8Value size))
      (do innerSize <- getWord8Bits
          Vector.replicateM
            (fromIntegral (word8Value innerSize))
            (do x <- getWord32Bits
                y <- getCompressedWord 27
                pure (x, y)))
  pure (LoadoutOnlineAttribute values)

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBit.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let values = loadoutAttributeValue loadoutAttribute
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
