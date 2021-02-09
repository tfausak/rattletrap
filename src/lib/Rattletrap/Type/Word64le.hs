module Rattletrap.Type.Word64le
  ( Word64le(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Word as Word
import qualified Text.Read as Read

newtype Word64le = Word64le
  { word64leValue :: Word.Word64
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Word64le where
  parseJSON value = case value of
    Aeson.String text -> case Read.readEither $ Text.unpack text of
      Left _ -> Aeson.typeMismatch "Word64le" value
      Right word64 -> pure $ Word64le word64
    Aeson.Number number -> case Scientific.toBoundedInteger number of
      Nothing -> Aeson.typeMismatch "Word64le" value
      Just word64 -> pure $ Word64le word64
    _ -> Aeson.typeMismatch "Word64le" value

instance Aeson.ToJSON Word64le where
  toJSON = Aeson.toJSON . show . word64leValue
