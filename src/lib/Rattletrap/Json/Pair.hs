module Rattletrap.Json.Pair where

import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

data Pair k v
    = Pair !k !v
    deriving (Eq, Show)

get :: TextGet.TextGet k -> TextGet.TextGet v -> TextGet.TextGet (Pair k v)
get f g = Pair
    <$> f <* TextGet.char ':' <* TextGet.spaces
    <*> g

put :: (k -> TextPut.TextPut) -> (v -> TextPut.TextPut) -> Pair k v -> TextPut.TextPut
put f g (Pair k v) = f k <> TextPut.char ':' <> g v
