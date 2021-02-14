{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Replication.Updated where

import qualified Rattletrap.Type.Attribute as Attribute
import Rattletrap.Type.Common
import Rattletrap.Decode.Common
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.BitPut as BitPut

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBits
import qualified Data.Map as Map

newtype Updated = Updated
  { attributes :: List.List Attribute.Attribute
  } deriving (Eq, Show)

$(deriveJson ''Updated)

bitPut :: Updated -> BitPut.BitPut
bitPut x = do
  Monad.forM_ (List.toList $ attributes x) $ \ y -> do
    BitPut.bool True
    Attribute.bitPut y
  BitPut.bool False

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet Updated
bitGet version classes actors actor = fmap Updated . List.untilM $ do
    p <- BinaryBits.getBool
    if p
      then Just <$> Attribute.bitGet version classes actors actor
      else pure Nothing
