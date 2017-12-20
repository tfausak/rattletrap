module Rattletrap.Decode.GameModeAttribute
  ( decodeGameModeAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.GameModeAttribute

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader

decodeGameModeAttributeBits
  :: Reader.ReaderT (Int, Int, Int) DecodeBits GameModeAttribute
decodeGameModeAttributeBits = do
  version <- Reader.ask
  GameModeAttribute <$> pure (numBits version) <*> Trans.lift
    (getWord8Bits (numBits version))

numBits :: (Int, Int, Int) -> Int
numBits version = if version < (868, 12, 0) then 2 else 8
