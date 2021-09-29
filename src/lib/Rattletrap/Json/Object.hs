module Rattletrap.Json.Object where

import qualified Data.Array as Array
import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

newtype Object p
    = Object (Array.Array Word p)
    deriving (Eq, Show)

get :: TextGet.TextGet p -> TextGet.TextGet (Object p)
get = fmap Object . TextGet.array
    (TextGet.char '{' *> TextGet.spaces)
    (TextGet.char ',' *> TextGet.spaces)
    (TextGet.char '}' *> TextGet.spaces)

put :: (p -> TextPut.TextPut) -> Object p -> TextPut.TextPut
put f (Object x) =
    TextPut.array (TextPut.char '{') (TextPut.char ',') (TextPut.char '}') f x
