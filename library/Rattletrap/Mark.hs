{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Mark where

import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data Mark = Mark
  { markValue :: Text
  , markFrame :: Word32
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Mark

instance Aeson.ToJSON Mark

getMark :: Binary.Get Mark
getMark = do
  value <- getText
  frame <- getWord32
  pure Mark {markValue = value, markFrame = frame}

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)
