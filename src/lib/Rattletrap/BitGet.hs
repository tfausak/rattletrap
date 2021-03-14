module Rattletrap.BitGet where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Functor.Identity as Identity
import qualified Rattletrap.BitString as BitString
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.Get as Get

type BitGet = Get.Get BitString.BitString Identity.Identity

toByteGet :: BitGet a -> ByteGet.ByteGet a
toByteGet g = do
  s1 <- Get.get
  case Identity.runIdentity . Get.run g $ BitString.fromByteString s1 of
    Left e -> ByteGet.throw e
    Right (s2, x) -> do
      Get.put $ BitString.byteString s2
      pure x

fromByteGet :: ByteGet.ByteGet a -> Int -> BitGet a
fromByteGet f n = do
  x <- byteString n
  Get.embed f x

bits :: Bits.Bits a => Int -> BitGet a
bits n = do
  let
    f :: Bits.Bits a => Bool -> a -> a
    f bit x = let y = Bits.shiftL x 1 in if bit then Bits.setBit y 0 else y
  xs <- Monad.replicateM n bool
  pure $ foldr f Bits.zeroBits xs

bool :: BitGet Bool
bool = do
  s1 <- Get.get
  case BitString.pop s1 of
    Nothing -> fail "BitGet.bool"
    Just (x, s2) -> do
      Get.put s2
      pure x

byteString :: Int -> BitGet ByteString.ByteString
byteString n = fmap ByteString.pack . Monad.replicateM n $ bits 8

throw :: Exception.Exception e => e -> BitGet a
throw = Get.throw
