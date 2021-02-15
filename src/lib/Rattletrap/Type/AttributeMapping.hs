{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.AttributeMapping where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32

data AttributeMapping = AttributeMapping
  { objectId :: U32.U32
  , streamId :: U32.U32
  }
  deriving (Eq, Show)

$(deriveJson ''AttributeMapping)

bytePut :: AttributeMapping -> BytePut.BytePut
bytePut x = U32.bytePut (objectId x) <> U32.bytePut (streamId x)

byteGet :: ByteGet.ByteGet AttributeMapping
byteGet = AttributeMapping <$> U32.byteGet <*> U32.byteGet
