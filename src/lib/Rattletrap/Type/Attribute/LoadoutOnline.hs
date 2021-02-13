{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.LoadoutOnline where

import Rattletrap.Type.Common
import Rattletrap.Type.ProductAttribute
import qualified Rattletrap.Type.Word8le as Word8le
import Rattletrap.Decode.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad
import qualified Data.Map as Map

newtype LoadoutOnlineAttribute = LoadoutOnlineAttribute
  { loadoutAttributeValue :: [[ProductAttribute]]
  } deriving (Eq, Show)

$(deriveJson ''LoadoutOnlineAttribute)

putLoadoutOnlineAttribute :: LoadoutOnlineAttribute -> BitPut ()
putLoadoutOnlineAttribute loadoutAttribute = do
  let attributes = loadoutAttributeValue loadoutAttribute
  Word8le.bitPut (Word8le.fromWord8 (fromIntegral (length attributes)))
  mapM_ putProductAttributes attributes

decodeLoadoutOnlineAttributeBits
  :: (Int, Int, Int)
  -> Map.Map Word32le Str
  -> BitGet LoadoutOnlineAttribute
decodeLoadoutOnlineAttributeBits version objectMap = do
  size <- Word8le.bitGet
  LoadoutOnlineAttribute <$> Monad.replicateM
    (fromIntegral (Word8le.toWord8 size))
    (decodeProductAttributesBits version objectMap)
