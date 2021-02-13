{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Mark where

import Rattletrap.Type.Common
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Decode.Common

import qualified Data.Binary as Binary

data Mark = Mark
  { markValue :: Str
  -- ^ Which type of mark this is, like @Team0Goal@.
  , markFrame :: Word32le
  -- ^ Which frame this mark belongs to, starting from 0.
  }
  deriving (Eq, Show)

$(deriveJson ''Mark)

putMark :: Mark -> Binary.Put
putMark mark = do
  putText (markValue mark)
  putWord32 (markFrame mark)

decodeMark :: Decode Mark
decodeMark = Mark <$> decodeStr <*> decodeWord32le
