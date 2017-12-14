module Rattletrap.Encode.StringAttribute
  ( putStringAttribute
  ) where

import Rattletrap.Type.StringAttribute
import Rattletrap.Encode.Text

import qualified Data.Binary.Bits.Put as BinaryBit

putStringAttribute :: StringAttribute -> BinaryBit.BitPut ()
putStringAttribute stringAttribute =
  putTextBits (stringAttributeValue stringAttribute)