{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Mark
  ( Mark(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le

data Mark = Mark
  { markValue :: Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , markFrame :: Word32le
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Mark)
