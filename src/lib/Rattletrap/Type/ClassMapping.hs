{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.ClassMapping where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary

data ClassMapping = ClassMapping
  { classMappingName :: Str
  , classMappingStreamId :: Word32le
  }
  deriving (Eq, Show)

$(deriveJson ''ClassMapping)

putClassMapping :: ClassMapping -> Binary.Put
putClassMapping classMapping = do
  putText (classMappingName classMapping)
  putWord32 (classMappingStreamId classMapping)

decodeClassMapping :: Decode ClassMapping
decodeClassMapping = ClassMapping <$> decodeStr <*> decodeWord32le
