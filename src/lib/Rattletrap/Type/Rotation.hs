module Rattletrap.Type.Rotation where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.CompressedWordVector as CompressedWordVector
import qualified Rattletrap.Type.Quaternion as Quaternion
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Rotation
  = CompressedWordVector CompressedWordVector.CompressedWordVector
  | Quaternion Quaternion.Quaternion
  deriving (Eq, Show)

instance Argo.HasCodec Rotation where
  codec =
    Argo.identified
      $ Argo.mapMaybe
          (Just . CompressedWordVector)
          (\x -> case x of
            CompressedWordVector y -> Just y
            _ -> Nothing
          )
          (Argo.fromObjectCodec
            Argo.Allow
            (Argo.required id "compressed_word_vector")
          )
      Argo.<|> Argo.mapMaybe
                 (Just . Quaternion)
                 (\x -> case x of
                   Quaternion y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required id "quaternion")
                 )

bitPut :: Rotation -> BitPut.BitPut
bitPut r = case r of
  CompressedWordVector cwv -> CompressedWordVector.bitPut cwv
  Quaternion q -> Quaternion.bitPut q

bitGet :: Version.Version -> BitGet.BitGet Rotation
bitGet version = if Version.atLeast 868 22 7 version
  then fmap Quaternion Quaternion.bitGet
  else fmap CompressedWordVector CompressedWordVector.bitGet
