{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Control.Monad as Monad

newtype List a
  = List [a]
  deriving (Eq, Show)

$(deriveJson ''List)

fromList :: [a] -> List a
fromList = List

toList :: List a -> [a]
toList (List x) = x

bytePut :: (a -> BytePut) -> List a -> BytePut
bytePut putElement list = do
  let elements = toList list
  Word32le.bytePut (Word32le.fromWord32 (fromIntegral (length elements)))
  mapM_ putElement elements

byteGet :: ByteGet a -> ByteGet (List a)
byteGet decodeElement = do
  size <- Word32le.byteGet
  List <$> Monad.replicateM (fromIntegral (Word32le.toWord32 size)) decodeElement
