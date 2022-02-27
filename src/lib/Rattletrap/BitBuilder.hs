module Rattletrap.BitBuilder where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.Word as Word

data BitBuilder = BitBuilder
  { buffer :: Word.Word8
  , builder :: Builder.Builder
  , offset :: Int
  }

empty :: BitBuilder
empty = BitBuilder { buffer = 0x00, builder = mempty, offset = 0 }

push :: Bool -> BitBuilder -> BitBuilder
push b x =
  let !newBuffer = if b then Bits.setBit (buffer x) (offset x) else buffer x
  in
    if offset x == 7
      then BitBuilder
        { buffer = 0x00
        , builder = builder x <> Builder.word8 newBuffer
        , offset = 0
        }
      else x { buffer = newBuffer, offset = offset x + 1 }

toBuilder :: BitBuilder -> Builder.Builder
toBuilder x =
  if offset x == 0 then builder x else builder x <> Builder.word8 (buffer x)
