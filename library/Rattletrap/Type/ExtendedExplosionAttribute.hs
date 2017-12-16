{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ExtendedExplosionAttribute
  ( ExtendedExplosionAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.ExplosionAttribute
import Rattletrap.Type.FlaggedIntAttribute

data ExtendedExplosionAttribute = ExtendedExplosionAttribute
  { extendedExplosionAttributeExplosion :: ExplosionAttribute
  , extendedExplosionAttributeUnknown :: FlaggedIntAttribute
  } deriving (Eq, Ord, Show)

$(deriveJson ''ExtendedExplosionAttribute)
