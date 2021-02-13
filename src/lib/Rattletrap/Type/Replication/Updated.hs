{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Updated where

import Rattletrap.Type.Attribute
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.CompressedWord
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Data.Map as Map

newtype Updated = Updated
  { attributes :: [Attribute]
  } deriving (Eq, Show)

$(deriveJsonWith ''Updated jsonOptions)

bitPut :: Updated -> BitPut ()
bitPut updatedReplication =
  putAttributes (attributes updatedReplication)

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap
  -> Map.Map CompressedWord Word32le.Word32le
  -> CompressedWord
  -> BitGet Updated
bitGet version classes actors actor =
  Updated <$> decodeAttributesBits version classes actors actor
