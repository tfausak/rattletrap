{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Section
  ( Section(..)
  , toSection
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Word32le
import Rattletrap.Utility.Crc

import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Lazy as LazyBytes

-- | A section is a large piece of a 'Rattletrap.Replay.Replay'. It has a
-- 32-bit size (in bytes), a 32-bit CRC (see "Rattletrap.Utility.Crc"), and then a
-- bunch of data (the body). This interface is provided so that you don't have
-- to think about the size and CRC.
data Section a = Section
  { sectionSize :: Word32le
  -- ^ read only
  , sectionCrc :: Word32le
  -- ^ read only
  , sectionBody :: a
  -- ^ The actual content in the section.
  }
  deriving (Eq, Ord, Show)

$(deriveJson ''Section)

toSection :: (a -> Binary.Put) -> a -> Section a
toSection encode body =
  let bytes = LazyBytes.toStrict . Binary.runPut $ encode body
  in
    Section
      { sectionSize = Word32le . fromIntegral $ Bytes.length bytes
      , sectionCrc = Word32le $ getCrc32 bytes
      , sectionBody = body
      }
