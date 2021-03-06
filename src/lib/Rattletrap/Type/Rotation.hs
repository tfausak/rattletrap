module Rattletrap.Type.Rotation where

import qualified Data.Foldable as Foldable
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.CompressedWordVector as CompressedWordVector
import qualified Rattletrap.Type.Quaternion as Quaternion
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Rotation
  = CompressedWordVector CompressedWordVector.CompressedWordVector
  | Quaternion Quaternion.Quaternion
  deriving (Eq, Show)

instance Json.FromJSON Rotation where
  parseJSON = Json.withObject "Rotation" $ \object -> Foldable.asum
    [ fmap CompressedWordVector $ Json.required object "compressed_word_vector"
    , fmap Quaternion $ Json.required object "quaternion"
    ]

instance Json.ToJSON Rotation where
  toJSON x = case x of
    CompressedWordVector y ->
      Json.object [Json.pair "compressed_word_vector" y]
    Quaternion y -> Json.object [Json.pair "quaternion" y]

schema :: Schema.Schema
schema = Schema.named "rotation" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k $ Schema.ref v, True)])
  [ ("compressed_word_vector", CompressedWordVector.schema)
  , ("quaternion", Quaternion.schema)
  ]

bitPut :: Rotation -> BitPut.BitPut
bitPut r = case r of
  CompressedWordVector cwv -> CompressedWordVector.bitPut cwv
  Quaternion q -> Quaternion.bitPut q

bitGet :: Version.Version -> BitGet.BitGet Rotation
bitGet version = if isQuaternion version
  then fmap Quaternion Quaternion.bitGet
  else fmap CompressedWordVector CompressedWordVector.bitGet

isQuaternion :: Version.Version -> Bool
isQuaternion v =
  Version.major v >= 868 && Version.minor v >= 22 && Version.patch v >= 7
