module Rattletrap.Type.Int64le
  ( Int64le(..)
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Int as Int
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Text.Read as Read

newtype Int64le = Int64le
  { int64leValue :: Int.Int64
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Int64le where
  parseJSON value = case value of
    Aeson.String text -> case Read.readEither $ Text.unpack text of
      Left _ -> Aeson.typeMismatch "Int64le" value
      Right int64 -> pure $ Int64le int64
    Aeson.Number number -> case Scientific.toBoundedInteger number of
      Nothing -> Aeson.typeMismatch "Int64le" value
      Just int64 -> pure $ Int64le int64
    _ -> Aeson.typeMismatch "Int64le" value

instance Aeson.ToJSON Int64le where
  toJSON = Aeson.toJSON . show . int64leValue
