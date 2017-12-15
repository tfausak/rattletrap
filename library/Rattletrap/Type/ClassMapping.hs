{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClassMapping
  ( ClassMapping(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32
import Rattletrap.Type.Text

data ClassMapping = ClassMapping
  { classMappingName :: Text
  , classMappingStreamId :: Word32
  } deriving (Eq, Ord, Show)

$(deriveJson ''ClassMapping)
