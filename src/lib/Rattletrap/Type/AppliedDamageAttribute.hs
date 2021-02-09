{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AppliedDamageAttribute
  ( AppliedDamageAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector
import Rattletrap.Type.Word8le

data AppliedDamageAttribute = AppliedDamageAttribute
  { appliedDamageAttributeUnknown1 :: Word8le
  , appliedDamageAttributeLocation :: Vector
  , appliedDamageAttributeUnknown3 :: Int32le
  , appliedDamageAttributeUnknown4 :: Int32le
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''AppliedDamageAttribute)
