module Rattletrap.Decode.RemoteId
  ( decodeRemoteIdBits
  ) where

import Data.Semigroup ((<>))
import Rattletrap.Decode.Common
import Rattletrap.Decode.Word64le
import Rattletrap.Type.RemoteId
import Rattletrap.Type.Word8le
import Rattletrap.Utility.Bytes

import Control.Monad
import System.Environment
import System.IO.Unsafe
import Text.Read

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

decodeRemoteIdBits :: (Int, Int, Int) -> Word8le -> DecodeBits RemoteId
decodeRemoteIdBits (_, _, patch) systemId = case word8leValue systemId of
  0 -> RemoteIdSplitscreen <$> getWord32be 24
  1 -> RemoteIdSteam <$> decodeWord64leBits
  2 -> RemoteIdPlayStation <$> decodePsName <*> decodePsBytes patch
  4 -> RemoteIdXbox <$> decodeWord64leBits
  6 -> RemoteIdSwitch <$> decodeSwitchId
  _ -> fail ("unknown system id " <> show systemId)

decodePsName :: DecodeBits Text.Text
decodePsName = fmap
  ( Text.dropWhileEnd (== '\x00')
  . Text.decodeLatin1
  . LazyBytes.toStrict
  . reverseBytes
  )
  (getLazyByteStringBits 16)

decodePsBytes :: Int -> DecodeBits [Word.Word8]
decodePsBytes patch =
  LazyBytes.unpack <$> getLazyByteStringBits (if patch >= 1 then 24 else 16)

decodeSwitchId :: DecodeBits [Bool]
decodeSwitchId = replicateM numBits getBool

numBits :: Int
numBits = unsafePerformIO (do
  x <- lookupEnv "RATTLETRAP_NUM_BITS"
  case x of
    Nothing -> pure 0
    Just y -> case readMaybe y of
      Nothing -> pure 0
      Just z -> pure z)
{-# NOINLINE numBits #-}
