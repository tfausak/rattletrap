module Rattletrap.Json
    ( Value.Value(..)
    , get
    , Value.put
    , FromJson.FromJson(..)
    , ToJson.ToJson(..)
    ) where

import qualified Rattletrap.Json.FromJson as FromJson
import qualified Rattletrap.Json.ToJson as ToJson
import qualified Rattletrap.Json.Value as Value
import qualified Rattletrap.TextGet as TextGet

get :: TextGet.TextGet Value.Value
get = TextGet.spaces *> Value.get <* TextGet.eof
