{-# language NamedFieldPuns #-}

module Rattletrap.Type.Version ( Version(..), fromBytes, toBytes, fromJson, toJson ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Text as Text
import qualified Data.Word as Word

data Version = Version
  { major :: Word.Word32
  , minor :: Word.Word32
  , patch :: Maybe Word.Word32
  } deriving (Eq, Ord, Show)

instance Aeson.FromJSON Version where parseJSON = fromJson

instance Aeson.ToJSON Version where toJSON = toJson

fromBytes :: Binary.Get Version
fromBytes = Binary.label "Version" $ do
  major <- Binary.label "major" Binary.getWord32le
  minor <- Binary.label "minor" Binary.getWord32le
  patch <- Binary.label "patch" $ whenMaybe (hasPatch major minor) Binary.getWord32le
  pure Version { major, minor, patch }

toBytes :: Version -> Binary.Put
toBytes version = do
  Binary.putWord32le $ major version
  Binary.putWord32le $ minor version
  whenJust (patch version) Binary.putWord32le

fromJson :: Aeson.Value -> Aeson.Parser Version
fromJson = Aeson.withObject "Version" $ \ object -> do
  major <- required object "major"
  minor <- required object "minor"
  patch <- optional object "patch"
  pure Version { major, minor, patch }

toJson :: Version -> Aeson.Value
toJson version = Aeson.object
  [ pair "major" $ major version
  , pair "minor" $ minor version
  , pair "patch" $ patch version
  ]

hasPatch :: Word.Word32 -> Word.Word32 -> Bool
hasPatch major minor = major >= 868 && minor >= 18

whenMaybe :: Applicative f => Bool -> f a -> f (Maybe a)
whenMaybe p f = if p then fmap Just f else pure Nothing

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust m f = case m of
  Nothing -> pure ()
  Just x -> f x

required :: Aeson.FromJSON a => Aeson.Object -> String -> Aeson.Parser a
required object key = object Aeson..: Text.pack key

optional :: Aeson.FromJSON a => Aeson.Object -> String -> Aeson.Parser (Maybe a)
optional object key = object Aeson..:? Text.pack key

pair :: (Aeson.ToJSON v, Aeson.KeyValue p) => String -> v -> p
pair key value = Text.pack key Aeson..= value
