{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.CustomDemolishAttribute
  ( CustomDemolishAttribute(..)
  )
where

import Rattletrap.Type.Common
import Rattletrap.Type.DemolishAttribute
import Rattletrap.Type.Int32le

data CustomDemolishAttribute = CustomDemolishAttribute
  { customDemolishAttributeFlag :: Bool
  , customDemolishAttributeId :: Int32le
  , customDemolishAttributeDemolish :: DemolishAttribute
  } deriving (Eq, Ord, Show)

$(deriveJson ''CustomDemolishAttribute)
