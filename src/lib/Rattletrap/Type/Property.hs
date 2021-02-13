{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Property where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word64le as Word64le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Property = Property
  { kind :: Str.Str
  , size :: Word64le.Word64le
  -- ^ Not used.
  , value :: PropertyValue.PropertyValue Property
  }
  deriving (Eq, Show)

$(deriveJsonWith ''Property jsonOptions)

bytePut :: Property -> BytePut
bytePut property = do
  Str.bytePut (kind property)
  Word64le.bytePut (size property)
  PropertyValue.bytePut bytePut (value property)

byteGet :: ByteGet Property
byteGet = do
  kind_ <- Str.byteGet
  Property kind_ <$> Word64le.byteGet <*> PropertyValue.byteGet byteGet kind_
