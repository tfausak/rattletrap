{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.QWordAttribute
  ( QWordAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word64

newtype QWordAttribute = QWordAttribute
  { qWordAttributeValue :: Word64
  } deriving (Eq, Ord, Show)

$(deriveJson ''QWordAttribute)
