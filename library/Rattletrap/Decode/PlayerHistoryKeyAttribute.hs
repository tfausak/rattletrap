module Rattletrap.Decode.PlayerHistoryKeyAttribute
  ( decodePlayerHistoryKeyAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Type.PlayerHistoryKeyAttribute

import qualified Control.Monad as Monad

import System.Environment
import System.IO.Unsafe
import Text.Read

decodePlayerHistoryKeyAttributeBits :: DecodeBits PlayerHistoryKeyAttribute
decodePlayerHistoryKeyAttributeBits = PlayerHistoryKeyAttribute <$> Monad.replicateM numBits getBool

numBits :: Int
numBits = unsafePerformIO (do
  m <- lookupEnv "RATTLETRAP_NUM_BITS"
  case m of
    Nothing -> pure 0
    Just s -> case readMaybe s of
      Nothing -> pure 0
      Just n -> pure n)
{-# NOINLINE numBits #-}
