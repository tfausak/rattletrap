{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.CustomDemolish where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import qualified Rattletrap.Type.Int32le as Int32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data CustomDemolishAttribute = CustomDemolishAttribute
  { flag :: Bool
  , id :: Int32le.Int32le
  , demolish :: Demolish.DemolishAttribute
  }
  deriving (Eq, Show)

$(deriveJsonWith ''CustomDemolishAttribute jsonOptions)

bitPut :: CustomDemolishAttribute -> BitPut ()
bitPut x = do
  BinaryBits.putBool (flag x)
  Int32le.bitPut (Rattletrap.Type.Attribute.CustomDemolish.id x)
  Demolish.bitPut (demolish x)

bitGet
  :: (Int, Int, Int) -> BitGet CustomDemolishAttribute
bitGet version =
  CustomDemolishAttribute
    <$> getBool
    <*> Int32le.bitGet
    <*> Demolish.bitGet version
