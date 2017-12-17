module Rattletrap.Decode.LoadoutsAttribute
  ( decodeLoadoutsAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.LoadoutAttribute
import Rattletrap.Type.LoadoutsAttribute

decodeLoadoutsAttributeBits :: DecodeBits LoadoutsAttribute
decodeLoadoutsAttributeBits =
  LoadoutsAttribute
    <$> decodeLoadoutAttributeBits
    <*> decodeLoadoutAttributeBits
