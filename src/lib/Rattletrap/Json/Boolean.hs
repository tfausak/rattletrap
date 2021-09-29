module Rattletrap.Json.Boolean where

import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

newtype Boolean
    = Boolean Bool
    deriving (Eq, Show)

get :: TextGet.TextGet Boolean
get = Boolean <$> TextGet.choice
    [ False <$ TextGet.string "false" <* TextGet.spaces
    , True <$ TextGet.string "true" <* TextGet.spaces
    ]

put :: Boolean -> TextPut.TextPut
put (Boolean x) = TextPut.string $ if x then "true" else "false"
