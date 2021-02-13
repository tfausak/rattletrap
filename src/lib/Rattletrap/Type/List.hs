{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Show)

$(deriveJson ''List)

putList :: (a -> BytePut) -> List a -> BytePut
putList putElement list = do
  let elements = listValue list
  putWord32 (Word32le (fromIntegral (length elements)))
  mapM_ putElement elements

decodeList :: ByteGet a -> ByteGet (List a)
decodeList decodeElement = do
  size <- decodeWord32le
  List <$> Monad.replicateM (fromIntegral (word32leValue size)) decodeElement
