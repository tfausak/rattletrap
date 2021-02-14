{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.FlaggedInt where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

data FlaggedInt = FlaggedInt
  { flag :: Bool
  , int :: I32.I32
  }
  deriving (Eq, Show)

$(deriveJson ''FlaggedInt)

bitPut :: FlaggedInt -> BitPut.BitPut
bitPut flaggedIntAttribute = do
  BitPut.bool (flag flaggedIntAttribute)
  I32.bitPut (int flaggedIntAttribute)

bitGet :: BitGet FlaggedInt
bitGet =
  FlaggedInt <$> getBool <*> I32.bitGet
