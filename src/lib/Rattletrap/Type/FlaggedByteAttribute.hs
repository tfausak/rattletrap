{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.FlaggedByteAttribute
  ( FlaggedByteAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word8le

data FlaggedByteAttribute = FlaggedByteAttribute
  { flaggedByteAttributeFlag :: Bool
  , flaggedByteAttributeByte :: Word8le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''FlaggedByteAttribute)
