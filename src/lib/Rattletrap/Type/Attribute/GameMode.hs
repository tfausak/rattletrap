module Rattletrap.Type.Attribute.GameMode where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data GameMode = GameMode
  { numBits :: Int
  -- ^ This field is guaranteed to be small. In other words, it won't overflow.
  -- It's stored as a regular 'Int' rather than something more precise like an
  -- 'Int8' because it just gets passed to functions that expect 'Int's.
  -- There's no reason to do a bunch of conversions.
  , word :: Word.Word8
  }
  deriving (Eq, Show)

instance Argo.HasCodec GameMode where
  codec = Argo.fromObjectCodec Argo.Allow $ GameMode
    <$> Argo.project numBits (Argo.required (Argo.fromString "num_bits") Argo.codec)
    <*> Argo.project word (Argo.required (Argo.fromString "word") Argo.codec)

bitPut :: GameMode -> BitPut.BitPut
bitPut gameModeAttribute = do
  BitPut.bits (numBits gameModeAttribute) (word gameModeAttribute)

bitGet :: Version.Version -> BitGet.BitGet GameMode
bitGet version = BitGet.label "GameMode" $ do
  let numBits = if Version.atLeast 868 12 0 version then 8 else 2 :: Int
  word <- BitGet.label "word" $ BitGet.bits numBits
  pure GameMode { numBits, word }
