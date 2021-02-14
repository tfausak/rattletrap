{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CustomDemolish where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import qualified Rattletrap.Type.I32 as I32
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data CustomDemolishAttribute = CustomDemolishAttribute
  { flag :: Bool
  , id :: I32.I32
  , demolish :: Demolish.DemolishAttribute
  }
  deriving (Eq, Show)

$(deriveJson ''CustomDemolishAttribute)

bitPut :: CustomDemolishAttribute -> BitPut ()
bitPut x = do
  BinaryBits.putBool (flag x)
  I32.bitPut (Rattletrap.Type.Attribute.CustomDemolish.id x)
  Demolish.bitPut (demolish x)

bitGet
  :: (Int, Int, Int) -> BitGet CustomDemolishAttribute
bitGet version =
  CustomDemolishAttribute
    <$> getBool
    <*> I32.bitGet
    <*> Demolish.bitGet version
