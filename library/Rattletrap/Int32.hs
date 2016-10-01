{-# LANGUAGE DeriveGeneric #-}

module Rattletrap.Int32 where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Int as Int
import qualified GHC.Generics as Generics

newtype Int32 = Int32
  { int32Value :: Int.Int32
  } deriving (Eq, Generics.Generic, Ord, Show)

instance Aeson.FromJSON Int32

instance Aeson.ToJSON Int32

getInt32 :: Binary.Get Int32
getInt32 = do
  int32 <- Binary.getInt32le
  pure (Int32 int32)

putInt32 :: Int32 -> Binary.Put
putInt32 (Int32 int32) = Binary.putInt32le int32
