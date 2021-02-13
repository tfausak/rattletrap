{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.String where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype StringAttribute = StringAttribute
  { stringAttributeValue :: Str.Str
  } deriving (Eq, Show)

$(deriveJson ''StringAttribute)

putStringAttribute :: StringAttribute -> BitPut ()
putStringAttribute stringAttribute =
  Str.bitPut (stringAttributeValue stringAttribute)

decodeStringAttributeBits :: BitGet StringAttribute
decodeStringAttributeBits = StringAttribute <$> Str.bitGet
