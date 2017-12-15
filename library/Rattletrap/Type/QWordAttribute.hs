{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.QWordAttribute
  ( QWordAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64le

newtype QWordAttribute = QWordAttribute
  { qWordAttributeValue :: Word64le
  } deriving (Eq, Ord, Show)

$(deriveJson ''QWordAttribute)
