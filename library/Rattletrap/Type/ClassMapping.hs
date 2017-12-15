{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClassMapping
  ( ClassMapping(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Type.Str

data ClassMapping = ClassMapping
  { classMappingName :: Str
  , classMappingStreamId :: Word32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''ClassMapping)
