{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.List
  ( List(..)
  )
where

import Rattletrap.Type.Common

newtype List a = List
  { listValue :: [a]
  } deriving (Eq, Ord, Show)

$(deriveJson ''List)
