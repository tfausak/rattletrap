module Rattletrap.Json.Null where

import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

data Null
    = Null
    deriving (Eq, Show)

get :: TextGet.TextGet Null
get = Null <$ TextGet.string "null" <* TextGet.spaces

put :: Null -> TextPut.TextPut
put = const $ TextPut.string "null"
