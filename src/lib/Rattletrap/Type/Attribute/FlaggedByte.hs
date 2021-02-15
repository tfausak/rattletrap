{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedByte where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8

data FlaggedByte = FlaggedByte
  { flag :: Bool
  , byte :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedByte)

bitPut :: FlaggedByte -> BitPut.BitPut
bitPut flaggedByteAttribute = BitPut.bool (flag flaggedByteAttribute)
  <> U8.bitPut (byte flaggedByteAttribute)

bitGet :: BitGet.BitGet FlaggedByte
bitGet = FlaggedByte <$> BitGet.bool <*> U8.bitGet
