{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.PlayerHistoryKeyAttribute
  ( PlayerHistoryKeyAttribute(..)
  , Bits(..)
  ) where

import Rattletrap.Type.Common

import qualified Data.Aeson as Json
import qualified Data.Bool as Bool
import qualified Data.Text as Text

newtype Bits = Bits { unwrapBits :: [Bool] } deriving (Eq, Ord, Show)

instance Json.FromJSON Bits where
  parseJSON = Json.withText "Bits" (\ text -> Bits <$> mapM
    (\ char -> case char of
      '0' -> pure False
      '1' -> pure True
      _ -> fail ("invalid bit: " ++ show char))
    (Text.unpack text))

instance Json.ToJSON Bits where
  toJSON = Json.toJSON . map (Bool.bool '0' '1') . unwrapBits

data PlayerHistoryKeyAttribute = PlayerHistoryKeyAttribute
  { playerHistoryKeyAttributeValue :: Bits
  } deriving (Eq, Ord, Show)

$(deriveJson ''PlayerHistoryKeyAttribute)
