{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data AttributeMapping = AttributeMapping
  { objectId :: Word32le.Word32le
  , streamId :: Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''AttributeMapping)

bytePut :: AttributeMapping -> BytePut
bytePut attributeMapping = do
  Word32le.bytePut (objectId attributeMapping)
  Word32le.bytePut (streamId attributeMapping)

byteGet :: ByteGet AttributeMapping
byteGet =
  AttributeMapping <$> Word32le.byteGet <*> Word32le.byteGet
