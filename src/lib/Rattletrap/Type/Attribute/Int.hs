{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.Int where

import Prelude hiding (Int)
import Rattletrap.Type.Common
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

newtype Int = Int
  { value :: I32.I32
  } deriving (Eq, Show)

$(deriveJson ''Int)

bitPut :: Int -> BitPut.BitPut
bitPut intAttribute = I32.bitPut (value intAttribute)

bitGet :: BitGet.BitGet Int
bitGet = Int <$> I32.bitGet
