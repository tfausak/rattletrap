module Rattletrap.Json.Value where

import qualified Rattletrap.Json.Array as Array
import qualified Rattletrap.Json.Boolean as Boolean
import qualified Rattletrap.Json.Null as Null
import qualified Rattletrap.Json.Number as Number
import qualified Rattletrap.Json.Object as Object
import qualified Rattletrap.Json.Pair as Pair
import qualified Rattletrap.Json.String as String
import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

data Value
    = Null Null.Null
    | Boolean Boolean.Boolean
    | Number Number.Number
    | String String.String
    | Array (Array.Array Value)
    | Object (Object.Object (Pair.Pair String.String Value))
    deriving (Eq, Show)

get :: TextGet.TextGet Value
get = TextGet.choice
    [ Null <$> Null.get
    , Boolean <$> Boolean.get
    , Number <$> Number.get
    , String <$> String.get
    , Array <$> Array.get get
    , Object <$> Object.get (Pair.get String.get get)
    ]

put :: Value -> TextPut.TextPut
put x = case x of
    Null y -> Null.put y
    Boolean y -> Boolean.put y
    Number y -> Number.put y
    String y -> String.put y
    Array y -> Array.put put y
    Object y -> Object.put (Pair.put String.put put) y
