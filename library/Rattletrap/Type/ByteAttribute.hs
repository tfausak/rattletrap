module Rattletrap.Type.ByteAttribute
  ( ByteAttribute(..)
  ) where

import Rattletrap.Type.Word8

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8
  } deriving (Eq, Ord, Show)
