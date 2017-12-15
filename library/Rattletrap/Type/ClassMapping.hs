{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClassMapping
  ( ClassMapping(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Text

data ClassMapping = ClassMapping
  { classMappingName :: Text
  , classMappingStreamId :: Word32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''ClassMapping)
