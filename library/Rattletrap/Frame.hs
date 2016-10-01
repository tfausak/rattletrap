{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Frame where

import Rattletrap.ClassPropertyMap
import Rattletrap.Float32
import Rattletrap.Replication

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified GHC.Generics as Generics

data Frame = Frame
  { frameTime :: Float32
  , frameDelta :: Float32
  , frameReplications :: [Replication]
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Frame

instance Aeson.ToJSON Frame

getFrames :: ClassPropertyMap -> BinaryBit.BitGet [Frame]
getFrames classPropertyMap = do
  maybeFrame <- getFrame classPropertyMap
  case maybeFrame of
    Nothing -> pure []
    Just frame -> do
      frames <- getFrames classPropertyMap
      pure (frame : frames)

putFrames :: [Frame] -> BinaryBit.BitPut ()
putFrames = mapM_ putFrame

getFrame :: ClassPropertyMap -> BinaryBit.BitGet (Maybe Frame)
getFrame classPropertyMap = do
  isEmpty <- BinaryBit.isEmpty
  if isEmpty
    then pure Nothing
    else do
      time <- getFloat32Bits
      delta <- getFloat32Bits
      if time == Float32 0 && delta == Float32 0
        then pure Nothing
        else do
          replications <- getReplications classPropertyMap
          pure
            (Just
               Frame
               { frameTime = time
               , frameDelta = delta
               , frameReplications = replications
               })

putFrame :: Frame -> BinaryBit.BitPut ()
putFrame frame = do
  putFloat32Bits (frameTime frame)
  putFloat32Bits (frameDelta frame)
  putReplications (frameReplications frame)
