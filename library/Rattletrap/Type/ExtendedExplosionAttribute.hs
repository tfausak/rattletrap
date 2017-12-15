{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ExtendedExplosionAttribute
  ( ExtendedExplosionAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32
import Rattletrap.Type.Vector

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeActorId :: Int32
  , extendedExplosionAttributeLocation :: Vector
  , extendedExplosionAttributeUnknown1 :: Bool
  , extendedExplosionAttributeUnknown2 :: Int32
  } deriving (Eq, Ord, Show)

$(deriveJson ''ExtendedExplosionAttribute)
