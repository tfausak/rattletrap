module Rattletrap.Decode.LoadoutOnlineAttribute
  ( decodeLoadoutOnlineAttributeBits
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.ProductAttribute
import Rattletrap.Decode.Word8le
import Rattletrap.Type.LoadoutOnlineAttribute
import Rattletrap.Type.Str
import Rattletrap.Type.Word32le
import Rattletrap.Type.Word8le

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Map as Map

decodeLoadoutOnlineAttributeBits
  :: Reader.ReaderT
       ((Int, Int, Int), Map.Map Word32le Str)
       DecodeBits
       LoadoutOnlineAttribute
decodeLoadoutOnlineAttributeBits = do
  size <- Trans.lift decodeWord8leBits
  (version, objectMap) <- Reader.ask
  LoadoutOnlineAttribute <$> Monad.replicateM
    (fromIntegral (word8leValue size))
    (Trans.lift (decodeProductAttributesBits version objectMap))
