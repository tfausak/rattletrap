{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Str
  ( Str(..)
  , toStr
  , fromStr
  )
where

import Rattletrap.Type.Common

import qualified Data.Text as Text

newtype Str = Str
  { strValue :: Text
  } deriving (Eq, Ord, Show)

$(deriveJson ''Str)

toStr :: String -> Str
toStr string = Str (Text.pack string)

fromStr :: Str -> String
fromStr text = Text.unpack (strValue text)
