{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Mark
  ( Mark(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Text
import Rattletrap.Type.Word32

data Mark = Mark
  { markValue :: Text
  -- ^ Which type of mark this is, like @Team0Goal@.
  , markFrame :: Word32
  -- ^ Which frame this mark belongs to, starting from 0.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Mark where
  parseJSON = defaultParseJson "Mark"

instance ToJSON Mark where
  toEncoding = defaultToEncoding "Mark"
  toJSON = defaultToJson "Mark"
