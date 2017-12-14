module Rattletrap.Type.RemoteId
  ( RemoteId(..)
  ) where

import Rattletrap.Type.Word64

import qualified Data.Text as Text
import qualified Data.Word as Word

data RemoteId
  = PlayStationId Text.Text
                  [Word.Word8]
  | SplitscreenId Word.Word32
  | SteamId Word64
  | XboxId Word64
  deriving (Eq, Ord, Show)
