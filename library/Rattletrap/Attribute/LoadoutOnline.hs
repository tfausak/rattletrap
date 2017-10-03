module Rattletrap.Attribute.LoadoutOnline where

import Rattletrap.Attribute.ProductAttribute
import Rattletrap.Primitive

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Map as Map

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
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

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BinaryBit.BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let attributes = loadoutAttributeValue loadoutAttribute
  putWord8Bits (Word8 (fromIntegral (length attributes)))
  mapM_ putProductAttributes attributes
