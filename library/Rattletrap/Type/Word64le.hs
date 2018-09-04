{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Word64le
  ( Word64le(..)
  )
where

import Rattletrap.Type.Common

newtype Word64le = Word64le
  { word64leValue :: Word64
  } deriving (Eq, Ord, Show)

$(deriveJson ''Word64le)
