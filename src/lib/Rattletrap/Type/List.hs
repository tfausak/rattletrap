{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
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
  Word32le.bytePut (Word32le.fromWord32 (fromIntegral (length elements)))
  mapM_ putElement elements

decodeList :: ByteGet a -> ByteGet (List a)
decodeList decodeElement = do
  size <- Word32le.byteGet
  List <$> Monad.replicateM (fromIntegral (Word32le.toWord32 size)) decodeElement
