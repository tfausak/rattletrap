module Rattletrap.Json.Array where

import qualified Data.Array as Array
import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

newtype Array a
    = Array (Array.Array Word a)
    deriving (Eq, Show)

get :: TextGet.TextGet a -> TextGet.TextGet (Array a)
get = fmap Array . TextGet.array
    (TextGet.char '[' *> TextGet.spaces)
    (TextGet.char ',' *> TextGet.spaces)
    (TextGet.char ']' *> TextGet.spaces)

put :: (a -> TextPut.TextPut) -> Array a -> TextPut.TextPut
put f (Array x) =
    TextPut.array (TextPut.char '[') (TextPut.char ',') (TextPut.char ']') f x
