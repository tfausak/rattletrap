{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Mark where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.ByteGet as ByteGet

data Mark = Mark
  { value :: Str.Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , frame :: U32.U32
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Show)

$(deriveJson ''Mark)

bytePut :: Mark -> BytePut.BytePut
bytePut x =
  Str.bytePut (value x)
  <> U32.bytePut (frame x)

byteGet :: ByteGet.ByteGet Mark
byteGet = Mark <$> Str.byteGet <*> U32.byteGet
