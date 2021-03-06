module Rattletrap.Type.Version where

import qualified Rattletrap.Type.U32 as U32

data Version = Version
  { major :: U32.U32
  , minor :: U32.U32
  , patch :: Maybe U32.U32
  }
  deriving (Eq, Show)

atLeast :: Int -> Int -> Int -> Version -> Bool
atLeast m n p v =
  U32.toWord32 (major v) >= fromIntegral m &&
  U32.toWord32 (minor v) >= fromIntegral n &&
  maybe 0 U32.toWord32 (patch v) >= fromIntegral p
