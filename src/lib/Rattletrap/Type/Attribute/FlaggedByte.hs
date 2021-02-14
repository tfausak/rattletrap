{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedByte where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U8 as U8
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

data FlaggedByte = FlaggedByte
  { flag :: Bool
  , byte :: U8.U8
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedByte)

bitPut :: FlaggedByte -> BitPut.BitPut
bitPut flaggedByteAttribute = do
  BitPut.bool (flag flaggedByteAttribute)
  U8.bitPut (byte flaggedByteAttribute)

bitGet :: BitGet FlaggedByte
bitGet =
  FlaggedByte <$> getBool <*> U8.bitGet
