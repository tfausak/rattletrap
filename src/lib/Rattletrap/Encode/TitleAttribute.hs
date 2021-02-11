module Rattletrap.Encode.TitleAttribute
  ( putTitleAttribute
  ) where

import Rattletrap.Type.Word32le
import Rattletrap.Type.TitleAttribute

import qualified Data.Binary.Bits.Put as BinaryBits

putTitleAttribute :: TitleAttribute -> BinaryBits.BitPut ()
putTitleAttribute titleAttribute = do
  BinaryBits.putBool (titleAttributeUnknown1 titleAttribute)
  BinaryBits.putBool (titleAttributeUnknown2 titleAttribute)
  putWord32Bits (titleAttributeUnknown3 titleAttribute)
  putWord32Bits (titleAttributeUnknown4 titleAttribute)
  putWord32Bits (titleAttributeUnknown5 titleAttribute)
  putWord32Bits (titleAttributeUnknown6 titleAttribute)
  putWord32Bits (titleAttributeUnknown7 titleAttribute)
  BinaryBits.putBool (titleAttributeUnknown8 titleAttribute)
