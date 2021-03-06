module Rattletrap.Type.Version where

import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.Utility.Monad as Monad

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

bytePut :: Version -> BytePut.BytePut
bytePut x = U32.bytePut (major x)
  <> U32.bytePut (minor x)
  <> foldMap U32.bytePut (patch x)

byteGet :: ByteGet.ByteGet Version
byteGet = do
  major <- U32.byteGet
  minor <- U32.byteGet
  patch <- Monad.whenMaybe
    (U32.toWord32 major >= 868 && U32.toWord32 minor >= 18)
    U32.byteGet
  pure Version { major, minor, patch }
