module Rattletrap.Encode.StringAttribute
  ( putStringAttribute
  )
where

import Rattletrap.Encode.Str
import Rattletrap.Type.StringAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putStringAttribute :: StringAttribute -> BinaryBits.BitPut ()
putStringAttribute stringAttribute =
  putTextBits (stringAttributeValue stringAttribute)
