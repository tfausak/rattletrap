module Rattletrap.TextPut where

import qualified Data.Array as Array
import qualified Data.List as List
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

type TextPut = Builder.Builder

array
    :: TextPut -- left, like [
    -> TextPut -- separator, like ,
    -> TextPut -- right, like ]
    -> (a -> TextPut)
    -> Array.Array i a
    -> TextPut
array l s r f = mappend l . foldr mappend r . List.intersperse s . fmap f . Array.elems

char :: Char -> TextPut
char = Builder.singleton

integer :: Integer -> TextPut
integer = Builder.decimal

string :: String -> TextPut
string = Builder.fromString
