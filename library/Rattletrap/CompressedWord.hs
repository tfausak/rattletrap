module Rattletrap.CompressedWord where

import Rattletrap.Utility

import qualified Control.Monad as Monad
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Bits as Bits

data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Ord, Show)

getCompressedWord :: Word -> BinaryBit.BitGet CompressedWord
getCompressedWord limit = do
  let step = compressedWordStep (const BinaryBit.getBool) limit
  value <- Monad.foldM step 0 [0 .. logBase2 limit]
  pure CompressedWord {compressedWordLimit = limit, compressedWordValue = value}

putCompressedWord :: CompressedWord -> BinaryBit.BitPut ()
putCompressedWord compressedWord = do
  let limit = compressedWordLimit compressedWord
  let value = compressedWordValue compressedWord
  let step =
        compressedWordStep
          (\index -> do
             let hasBit = Bits.testBit value index
             BinaryBit.putBool hasBit
             pure hasBit)
          limit
  Monad.foldM_ step 0 [0 .. logBase2 limit]

compressedWordStep
  :: (Bits.Bits a, Ord a, Num a, Monad m)
  => (Int -> m Bool) -> a -> a -> Int -> m a
compressedWordStep checkBit limit current index = do
  let bit = Bits.shiftL 1 index
  if current + bit >= limit
    then pure current
    else do
      hasBit <- checkBit index
      if hasBit
        then pure (current + bit)
        else pure current
