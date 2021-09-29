module Rattletrap.Json.FromJson where

import qualified Rattletrap.Json.Value as Value

class FromJson a where
    fromJson :: Value.Value -> Maybe a

instance FromJson Value.Value where
    fromJson = Just
