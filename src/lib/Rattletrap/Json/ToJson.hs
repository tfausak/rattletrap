module Rattletrap.Json.ToJson where

import qualified Rattletrap.Json.Value as Value

class ToJson a where
    toJson :: a -> Value.Value

instance ToJson Value.Value where
    toJson = id
