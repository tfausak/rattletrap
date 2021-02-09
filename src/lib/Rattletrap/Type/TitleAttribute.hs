{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.TitleAttribute
  ( TitleAttribute(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le

data TitleAttribute = TitleAttribute
  { titleAttributeUnknown1 :: Bool
  , titleAttributeUnknown2 :: Bool
  , titleAttributeUnknown3 :: Word32le
  , titleAttributeUnknown4 :: Word32le
  , titleAttributeUnknown5 :: Word32le
  , titleAttributeUnknown6 :: Word32le
  , titleAttributeUnknown7 :: Word32le
  , titleAttributeUnknown8 :: Bool
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''TitleAttribute)
