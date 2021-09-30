module Rattletrap.Json
    ( Value.Value(..)
    , get
    , Value.put
    ) where

import qualified Rattletrap.Json.Value as Value
import qualified Rattletrap.TextGet as TextGet

get :: TextGet.TextGet Value.Value
get = TextGet.spaces *> Value.get <* TextGet.eof
