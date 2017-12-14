module Rattletrap.Type.AppliedDamageAttribute
  ( AppliedDamageAttribute(..)
  ) where

import Rattletrap.Type.Word8
import Rattletrap.Type.Vector
import Rattletrap.Type.Int32

data AppliedDamageAttribute = AppliedDamageAttribute
  { appliedDamageAttributeUnknown1 :: Word8
  , appliedDamageAttributeLocation :: Vector
  , appliedDamageAttributeUnknown3 :: Int32
  , appliedDamageAttributeUnknown4 :: Int32
  } deriving (Eq, Ord, Show)
