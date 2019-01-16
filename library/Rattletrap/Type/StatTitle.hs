{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.StatTitle
  ( StatTitle(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.FlaggedIntAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data StatTitle = StatTitle
  { statTitleUnknown :: Bool
  , statTitleName :: Str
  , statTitleObjectTarget :: FlaggedIntAttribute
  , statTitleValue :: Word32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''StatTitle)
