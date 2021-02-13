{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Control.Monad as Monad
import qualified Data.Binary as Binary

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Show)

$(deriveJson ''List)

putList :: (a -> Binary.Put) -> List a -> Binary.Put
putList putElement list = do
  let elements = listValue list
  putWord32 (Word32le (fromIntegral (length elements)))
  mapM_ putElement elements

decodeList :: ByteGet a -> ByteGet (List a)
decodeList decodeElement = do
  size <- decodeWord32le
  List <$> Monad.replicateM (fromIntegral (word32leValue size)) decodeElement
