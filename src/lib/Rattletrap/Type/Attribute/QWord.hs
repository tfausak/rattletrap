{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.QWord where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U64 as U64
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype QWord = QWord
  { value :: U64.U64
  } deriving (Eq, Show)

$(deriveJson ''QWord)

bitPut :: QWord -> BitPut.BitPut
bitPut qWordAttribute =
  U64.bitPut (value qWordAttribute)

bitGet :: BitGet QWord
bitGet = QWord <$> U64.bitGet
