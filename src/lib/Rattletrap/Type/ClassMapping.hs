{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClassMapping where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data ClassMapping = ClassMapping
  { name :: Str.Str
  , streamId :: Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJsonWith ''ClassMapping jsonOptions)

bytePut :: ClassMapping -> BytePut
bytePut classMapping = do
  Str.bytePut (name classMapping)
  Word32le.bytePut (streamId classMapping)

byteGet :: ByteGet ClassMapping
byteGet = ClassMapping <$> Str.byteGet <*> Word32le.byteGet
