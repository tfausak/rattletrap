{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
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
  U32.bytePut (U32.fromWord32 (fromIntegral (length elements)))
  mapM_ putElement elements

byteGet :: ByteGet a -> ByteGet (List a)
byteGet decodeElement = do
  size <- U32.byteGet
  List <$> Monad.replicateM (fromIntegral (U32.toWord32 size)) decodeElement
