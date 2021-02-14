{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Updated where

import qualified Rattletrap.Type.Attribute as Attribute
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Encode.Common

import qualified Data.Map as Map

newtype Updated = Updated
  { attributes :: [Attribute.Attribute]
  } deriving (Eq, Show)

$(deriveJson ''Updated)

bitPut :: Updated -> BitPut ()
bitPut updatedReplication =
  Attribute.putAttributes (attributes updatedReplication)

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet Updated
bitGet version classes actors actor =
  Updated <$> Attribute.decodeAttributesBits version classes actors actor
