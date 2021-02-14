{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

$(deriveJson ''AttributeMapping)

bytePut :: AttributeMapping -> BytePut
bytePut attributeMapping = do
  U32.bytePut (objectId attributeMapping)
  U32.bytePut (streamId attributeMapping)

byteGet :: ByteGet AttributeMapping
byteGet =
  AttributeMapping <$> U32.byteGet <*> U32.byteGet
