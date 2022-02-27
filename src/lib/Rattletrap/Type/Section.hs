module Rattletrap.Type.Section where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Exception.CrcMismatch as CrcMismatch
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Crc as Crc
import qualified Rattletrap.Vendor.Argo as Argo

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

instance Argo.HasCodec a => Argo.HasCodec (Section a) where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ Section
      <$> Argo.required size "size"
      <*> Argo.required crc "crc"
      <*> Argo.required body "body"

create :: (a -> BytePut.BytePut) -> a -> Section a
create encode body_ =
  let bytes = BytePut.toByteString $ encode body_
  in
    Section
      { size = U32.fromWord32 . fromIntegral $ ByteString.length bytes
      , crc = U32.fromWord32 $ Crc.compute bytes
      , body = body_
      }

-- | Given a way to put the 'body', puts a section. This will also put
-- the size and CRC.
bytePut :: (a -> BytePut.BytePut) -> Section a -> BytePut.BytePut
bytePut putBody section =
  let
    rawBody = BytePut.toByteString . putBody $ body section
    size_ = ByteString.length rawBody
    crc_ = Crc.compute rawBody
  in
    U32.bytePut (U32.fromWord32 (fromIntegral size_))
    <> U32.bytePut (U32.fromWord32 crc_)
    <> BytePut.byteString rawBody

byteGet
  :: Bool -> (U32.U32 -> ByteGet.ByteGet a) -> ByteGet.ByteGet (Section a)
byteGet skip getBody = ByteGet.label "Section" $ do
  size <- ByteGet.label "size" U32.byteGet
  crc <- ByteGet.label "crc" U32.byteGet
  body <- ByteGet.label "body" $ do
    rawBody <- ByteGet.byteString . fromIntegral $ U32.toWord32 size
    Monad.unless skip $ do
      let
        expected = U32.toWord32 crc
        actual = Crc.compute rawBody
      Monad.when (actual /= expected) . ByteGet.throw $ CrcMismatch.CrcMismatch
        expected
        actual
    ByteGet.embed (getBody size) rawBody
  pure Section { size, crc, body }
