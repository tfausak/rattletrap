{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.String where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype StringAttribute = StringAttribute
  { value :: Str.Str
  } deriving (Eq, Show)

$(deriveJson ''StringAttribute)

bitPut :: StringAttribute -> BitPut ()
bitPut stringAttribute =
  Str.bitPut (value stringAttribute)

bitGet :: BitGet StringAttribute
bitGet = StringAttribute <$> Str.bitGet
