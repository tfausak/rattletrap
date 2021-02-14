{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.WeldedInfo where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Float32le as Float32le
import qualified Rattletrap.Type.Int32le as Int32le
import qualified Rattletrap.Type.Int8Vector as Int8Vector
import qualified Rattletrap.Type.Vector as Vector
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

data WeldedInfoAttribute = WeldedInfoAttribute
  { active :: Bool
  , actorId :: Int32le.Int32le
  , offset :: Vector.Vector
  , mass :: Float32le.Float32le
  , rotation :: Int8Vector.Int8Vector
  }
  deriving (Eq, Show)

$(deriveJsonWith ''WeldedInfoAttribute jsonOptions)

bitPut :: WeldedInfoAttribute -> BitPut ()
bitPut weldedInfoAttribute = do
  BinaryBits.putBool (active weldedInfoAttribute)
  Int32le.bitPut (actorId weldedInfoAttribute)
  Vector.bitPut (offset weldedInfoAttribute)
  Float32le.bitPut (mass weldedInfoAttribute)
  Int8Vector.bitPut (rotation weldedInfoAttribute)

bitGet
  :: (Int, Int, Int) -> BitGet WeldedInfoAttribute
bitGet version =
  WeldedInfoAttribute
    <$> getBool
    <*> Int32le.bitGet
    <*> Vector.bitGet version
    <*> Float32le.bitGet
    <*> Int8Vector.bitGet
