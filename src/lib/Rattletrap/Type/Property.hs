{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Property where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.PropertyValue as PropertyValue
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U64 as U64

data Property = Property
  { kind :: Str.Str
  , size :: U64.U64
  -- ^ Not used.
  , value :: PropertyValue.PropertyValue Property
  }
  deriving (Eq, Show)

$(deriveJson ''Property)

bytePut :: Property -> BytePut.BytePut
bytePut x =
  do
      Str.bytePut (kind x)
    <> U64.bytePut (size x)
    <> PropertyValue.bytePut bytePut (value x)

byteGet :: ByteGet.ByteGet Property
byteGet = do
  kind_ <- Str.byteGet
  Property kind_ <$> U64.byteGet <*> PropertyValue.byteGet byteGet kind_
