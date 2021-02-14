{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Section where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Crc as Crc
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.ByteGet as ByteGet

import qualified Control.Monad as Monad
import qualified Data.ByteString as Bytes

-- | A section is a large piece of a 'Rattletrap.Replay.Replay'. It has a
-- 32-bit size (in bytes), a 32-bit CRC (see "Rattletrap.Utility.Crc"), and then a
-- bunch of data (the body). This interface is provided so that you don't have
-- to think about the size and CRC.
data Section a = Section
  { size :: U32.U32
  -- ^ read only
  , crc :: U32.U32
  -- ^ read only
  , body :: a
  -- ^ The actual content in the section.
  }
  deriving (Eq, Show)

$(deriveJson ''Section)

create :: (a -> BytePut.BytePut) -> a -> Section a
create encode body_ =
  let bytes = BytePut.toByteString $ encode body_
  in
    Section
      { size = U32.fromWord32 . fromIntegral $ Bytes.length bytes
      , crc = U32.fromWord32 $ Crc.compute bytes
      , body = body_
      }

-- | Given a way to put the 'body', puts a section. This will also put
-- the size and CRC.
bytePut :: (a -> BytePut.BytePut) -> Section a -> BytePut.BytePut
bytePut putBody section =
  let
    rawBody = BytePut.toByteString . putBody $ body section
    size_ = Bytes.length rawBody
    crc_ = Crc.compute rawBody
  in U32.bytePut (U32.fromWord32 (fromIntegral size_))
  <> U32.bytePut (U32.fromWord32 crc_)
  <> BytePut.byteString rawBody

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Section a)
byteGet getBody = do
  size_ <- U32.byteGet
  crc_ <- U32.byteGet
  rawBody <- ByteGet.byteString (fromIntegral (U32.toWord32 size_))
  let actualCrc = U32.fromWord32 (Crc.compute rawBody)
  Monad.when (actualCrc /= crc_) (fail (crcMessage actualCrc crc_))
  body_ <- either fail pure $ ByteGet.run getBody rawBody
  pure (Section size_ crc_ body_)

crcMessage :: U32.U32 -> U32.U32 -> String
crcMessage actual expected = unwords
  [ "[RT10] actual CRC"
  , show actual
  , "does not match expected CRC"
  , show expected
  ]
