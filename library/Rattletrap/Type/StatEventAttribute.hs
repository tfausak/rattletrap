{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.StatEventAttribute
  ( StatEventAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le

data StatEventAttribute = StatEventAttribute
  { statEventAttributeUnknown :: Bool
  , statEventAttributeObjectId :: Int32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''StatEventAttribute)
