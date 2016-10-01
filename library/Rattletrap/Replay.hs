{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Replay where

import Rattletrap.Content
import Rattletrap.Header
import Rattletrap.Word32

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics

data Replay = Replay
  { replayHeaderSize :: Word32
  , replayHeaderCrc :: Word32
  , replayHeader :: Header
  , replayContentSize :: Word32
  , replayContentCrc :: Word32
  , replayContent :: Content
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Replay

instance Aeson.ToJSON Replay

getReplay :: Binary.Get Replay
getReplay = do
  headerSize <- getWord32
  headerCrc <- getWord32
  header <- getHeader
  contentSize <- getWord32
  contentCrc <- getWord32
  content <- getContent
  pure
    Replay
    { replayHeaderSize = headerSize
    , replayHeaderCrc = headerCrc
    , replayHeader = header
    , replayContentSize = contentSize
    , replayContentCrc = contentCrc
    , replayContent = content
    }

putReplay :: Replay -> Binary.Put
putReplay replay = do
  putWord32 (replayHeaderSize replay)
  putWord32 (replayHeaderCrc replay)
  putHeader (replayHeader replay)
  putWord32 (replayContentSize replay)
  putWord32 (replayContentCrc replay)
  putContent (replayContent replay)
