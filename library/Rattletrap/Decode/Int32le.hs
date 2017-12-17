module Rattletrap.Decode.Int32le
  ( decodeInt32le
  , decodeInt32leBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.Int32le

import qualified Data.Binary.Get as Binary

decodeInt32le :: Decode Int32le
decodeInt32le = Int32le <$> Binary.getInt32le

decodeInt32leBits :: DecodeBits Int32le
decodeInt32leBits = toBits decodeInt32le 4
