{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ExtendedExplosionAttribute
  ( ExtendedExplosionAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Int32le
import Rattletrap.Type.Vector

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeActorId :: Int32le
  , extendedExplosionAttributeLocation :: Vector
  , extendedExplosionAttributeUnknown1 :: Bool
  , extendedExplosionAttributeUnknown2 :: Int32le
  } deriving (Eq, Ord, Show)

$(deriveJson ''ExtendedExplosionAttribute)
