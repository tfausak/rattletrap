{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.String where

import Prelude hiding (String)
import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import Rattletrap.Decode.Common
import qualified Rattletrap.BitPut as BitPut

newtype String = String
  { value :: Str.Str
  } deriving (Eq, Show)

$(deriveJson ''String)

bitPut :: String -> BitPut.BitPut
bitPut stringAttribute =
  Str.bitPut (value stringAttribute)

bitGet :: BitGet String
bitGet = String <$> Str.bitGet
