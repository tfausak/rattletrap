{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Updated where

import qualified Rattletrap.Type.Attribute as Attribute
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Encode.Common

import qualified Data.Map as Map

newtype Updated = Updated
  { attributes :: [Attribute.Attribute]
  } deriving (Eq, Show)

$(deriveJsonWith ''Updated jsonOptions)

bitPut :: Updated -> BitPut ()
bitPut updatedReplication =
  Attribute.putAttributes (attributes updatedReplication)

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord Word32le.Word32le
  -> CompressedWord.CompressedWord
  -> BitGet Updated
bitGet version classes actors actor =
  Updated <$> Attribute.decodeAttributesBits version classes actors actor
