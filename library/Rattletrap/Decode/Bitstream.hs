module Rattletrap.Decode.Bitstream
  ( decodeBitstreamBits
  )
where

import Rattletrap.Decode.Common
import Rattletrap.Type.Bitstream

import qualified Control.Monad as Monad

decodeBitstreamBits :: Int -> DecodeBits Bitstream
decodeBitstreamBits n = Bitstream <$> Monad.replicateM n getBool
