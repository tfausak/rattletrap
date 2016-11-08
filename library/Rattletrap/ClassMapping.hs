module Rattletrap.ClassMapping where

import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Binary as Binary

data ClassMapping = ClassMapping
  { classMappingName :: Text
  , classMappingStreamId :: Word32
  } deriving (Eq, Ord, Show)

getClassMapping :: Binary.Get ClassMapping
getClassMapping = do
  name <- getText
  streamId <- getWord32
  pure (ClassMapping name streamId)

putClassMapping :: ClassMapping -> Binary.Put
putClassMapping classMapping = do
  putText (classMappingName classMapping)
  putWord32 (classMappingStreamId classMapping)
