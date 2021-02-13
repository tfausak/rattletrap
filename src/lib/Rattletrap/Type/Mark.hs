{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Mark where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.Word32le as Word32le
import Rattletrap.Decode.Common
import Rattletrap.Encode.Common

data Mark = Mark
  { markValue :: Str.Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , markFrame :: Word32le.Word32le
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Show)

$(deriveJson ''Mark)

putMark :: Mark -> BytePut
putMark mark = do
  Str.bytePut (markValue mark)
  Word32le.bytePut (markFrame mark)

decodeMark :: ByteGet Mark
decodeMark = Mark <$> Str.byteGet <*> Word32le.byteGet
