{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Mark where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Mark = Mark
  { value :: Str.Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , frame :: Word32le.Word32le
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Show)

$(deriveJson ''Mark)

bytePut :: Mark -> BytePut
bytePut mark = do
  Str.bytePut (value mark)
  Word32le.bytePut (frame mark)

byteGet :: ByteGet Mark
byteGet = Mark <$> Str.byteGet <*> Word32le.byteGet
