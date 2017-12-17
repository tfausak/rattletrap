module Rattletrap.Decode.List
  ( decodeList
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Word32le
import Rattletrap.Type.List
import Rattletrap.Type.Word32le

import qualified Control.Monad as Monad

decodeList :: Decode a -> Decode (List a)
decodeList decodeElement = do
  size <- decodeWord32le
  List <$> Monad.replicateM (fromIntegral (word32leValue size)) decodeElement
