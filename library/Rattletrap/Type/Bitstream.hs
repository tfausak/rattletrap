module Rattletrap.Type.Bitstream
  ( Bitstream(..)
  ) where

import qualified Data.Aeson as Json
import qualified Data.Bool as Bool
import qualified Data.Text as Text

newtype Bitstream = Bitstream
  { bitstreamValue :: [Bool]
  } deriving (Eq, Ord, Show)

instance Json.FromJSON Bitstream where
  parseJSON =
    Json.withText
      "Bitstream"
      (\text ->
         Bitstream <$>
         mapM
           (\char ->
              case char of
                '0' -> pure False
                '1' -> pure True
                _ -> fail ("invalid bit: " ++ show char))
           (Text.unpack text))

instance Json.ToJSON Bitstream where
  toJSON = Json.toJSON . fmap (Bool.bool '0' '1') . bitstreamValue
