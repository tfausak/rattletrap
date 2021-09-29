module Rattletrap.Json.String where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut
import qualified Text.Printf as Printf
import qualified Text.Read as Read

newtype String
    = String Text.Text
    deriving (Eq, Show)

get :: TextGet.TextGet Rattletrap.Json.String.String
get = do
    TextGet.char_ '"'
    xs <- TextGet.many Rattletrap.Json.String.getChar
    TextGet.char_ '"'
    TextGet.spaces
    pure . String . Text.pack $ pairSurrogates xs

getChar :: TextGet.TextGet Char
getChar = TextGet.choice [getLiteral, getEscape]

getLiteral :: TextGet.TextGet Char
getLiteral = TextGet.satisfy $ \ x -> x /= '"' && x /= '\\' && x > '\x1f'

getEscape :: TextGet.TextGet Char
getEscape = do
    TextGet.char_ '\\'
    x <- TextGet.satisfy $ const True
    case x of
        '"' -> pure '"'
        '\\' -> pure '\\'
        'b' -> pure '\b'
        'n' -> pure '\n'
        'r' -> pure '\r'
        't' -> pure '\t'
        'u' -> do
            y <- TextGet.count 4 $ TextGet.satisfy Char.isHexDigit
            case Read.readMaybe $ "0x" <> y of
                Nothing -> fail $ "getEscape: " <> show x
                Just z -> pure $ toEnum z
        _ -> fail $ "getEscape: " <> show x

pairSurrogates :: Prelude.String -> Prelude.String
pairSurrogates string = case string of
    hi : lo : rest
        | isHighSurrogate hi && isLowSurrogate lo ->
            pairSurrogate hi lo : pairSurrogates rest
    first : rest -> first : pairSurrogates rest
    "" -> string

isHighSurrogate :: Char -> Bool
isHighSurrogate x = '\xd800' <= x && x <= '\xdbff'

isLowSurrogate :: Char -> Bool
isLowSurrogate x = '\xdc00' <= x && x <= '\xdfff'

pairSurrogate :: Char -> Char -> Char
pairSurrogate hi lo =
    let
        x = 0x400 * (fromEnum hi - 0xd800)
        y = fromEnum lo - 0xdc00
    in toEnum $ 0x10000 + x + y

put :: Rattletrap.Json.String.String -> TextPut.TextPut
put (String x) = mappend (TextPut.char '"')
    $ Text.foldr (mappend . Rattletrap.Json.String.putChar) (TextPut.char '"') x

putChar :: Char -> TextPut.TextPut
putChar x = case x of
    '"' -> TextPut.string "\\\""
    '\\' -> TextPut.string "\\\\"
    '\b' -> TextPut.string "\\b"
    '\n' -> TextPut.string "\\n"
    '\r' -> TextPut.string "\\r"
    '\t' -> TextPut.string "\\t"
    _
        | x <= '\x1f' -> TextPut.string $ Printf.printf "\\u%04x" x
        | otherwise -> TextPut.char x
