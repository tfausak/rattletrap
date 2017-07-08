module Rattletrap.Attribute.LoadoutOnline where

import Rattletrap.Primitive

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[(Word32, Either CompressedWord Word32)]]
  } deriving (Eq, Ord, Show)

getLoadoutOnlineAttribute ::
     (Int, Int) -> BinaryBit.BitGet LoadoutOnlineAttribute
getLoadoutOnlineAttribute version = do
  let getOuter = do
        innerSize <- getWord8Bits
        Monad.replicateM (fromIntegral (word8Value innerSize)) getInner
      getInner = do
        x <- getWord32Bits
        y <- getY version
        pure (x, y)
      getY (major, minor) =
        if major >= 868 && minor >= 18
          then do
            y <- getWord32Bits
            pure (Right y)
          else do
            y <- getCompressedWord 27
            pure (Left y)
  size <- getWord8Bits
  values <- Monad.replicateM (fromIntegral (word8Value size)) getOuter
  pure (LoadoutOnlineAttribute values)

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBit.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let putOuter xs = do
        putWord8Bits (Word8 (fromIntegral (length xs)))
        mapM_ putInner xs
      putInner (x, y) = do
        putWord32Bits x
        putY y
      putY y =
        case y of
          Left l -> putCompressedWord l
          Right r -> putWord32Bits r
      value = loadoutAttributeValue loadoutAttribute
  putWord8Bits (Word8 (fromIntegral (length value)))
  mapM_ putOuter value
