{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ByteAttribute
  ( ByteAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le

newtype ByteAttribute = ByteAttribute
  { byteAttributeValue :: Word8le
  } deriving (Eq, Ord, Show)

$(deriveJson ''ByteAttribute)
