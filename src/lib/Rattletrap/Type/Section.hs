module Rattletrap.Type.Section where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Exception.CrcMismatch as CrcMismatch
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Crc as Crc
import qualified Rattletrap.Utility.Json as Json

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

instance Json.FromValue a => Json.FromValue (Section a) where
  fromValue = Json.withObject "Section" $ \object -> do
    size <- Json.required object "size"
    crc <- Json.required object "crc"
    body <- Json.required object "body"
    pure Section { size, crc, body }

instance Json.ToValue a => Json.ToValue (Section a) where
  toValue x = Json.object
    [ Json.pair "size" $ size x
    , Json.pair "crc" $ crc x
    , Json.pair "body" $ body x
    ]

schema :: Schema.Schema -> Schema.Schema
schema s =
  Schema.named ("section-" <> Text.unpack (Schema.name s)) $ Schema.object
    [ (Json.pair "size" $ Schema.ref U32.schema, True)
    , (Json.pair "crc" $ Schema.ref U32.schema, True)
    , (Json.pair "body" $ Schema.ref s, True)
    ]

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

crcMessage :: U32.U32 -> U32.U32 -> String
crcMessage actual expected = unwords
  [ "[RT10] actual CRC"
  , show actual
  , "does not match expected CRC"
  , show expected
  ]
