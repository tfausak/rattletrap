{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Type.Float32
  ( Float32(..)
  ) where

import Rattletrap.Type.Common

newtype Float32 = Float32
  { float32Value :: Float
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Float32 where
  parseJSON = defaultParseJson "Float32"

instance ToJSON Float32 where
  toEncoding = defaultToEncoding "Float32"
  toJSON = defaultToJson "Float32"
