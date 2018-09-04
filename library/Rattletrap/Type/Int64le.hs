{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Int64le
  ( Int64le(..)
  )
where

import Rattletrap.Type.Common

newtype Int64le = Int64le
  { int64leValue :: Int64
  } deriving (Eq, Ord, Show)

$(deriveJson ''Int64le)
