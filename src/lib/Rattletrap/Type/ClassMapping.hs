{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClassMapping where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data ClassMapping = ClassMapping
  { classMappingName :: Str.Str
  , classMappingStreamId :: Word32le.Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''ClassMapping)

putClassMapping :: ClassMapping -> BytePut
putClassMapping classMapping = do
  Str.bytePut (classMappingName classMapping)
  Word32le.bytePut (classMappingStreamId classMapping)

decodeClassMapping :: ByteGet ClassMapping
decodeClassMapping = ClassMapping <$> Str.byteGet <*> Word32le.byteGet
