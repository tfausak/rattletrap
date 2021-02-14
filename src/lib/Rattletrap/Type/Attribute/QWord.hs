{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.QWord where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U64 as U64
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

newtype QWordAttribute = QWordAttribute
  { value :: U64.U64
  } deriving (Eq, Show)

$(deriveJson ''QWordAttribute)

bitPut :: QWordAttribute -> BitPut ()
bitPut qWordAttribute =
  U64.bitPut (value qWordAttribute)

bitGet :: BitGet QWordAttribute
bitGet = QWordAttribute <$> U64.bitGet
