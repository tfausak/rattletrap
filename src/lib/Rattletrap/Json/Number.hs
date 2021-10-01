module Rattletrap.Json.Number where

import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import qualified Rattletrap.TextGet as TextGet
import qualified Rattletrap.TextPut as TextPut

data Number
    = Number !Integer !Integer
    deriving (Eq, Show)

toRational :: Number -> Rational
toRational (Number x y) = if y < 0 then x Ratio.% 10 ^ negate y else fromInteger $ x * 10 ^ y

toInteger :: Number -> Maybe Integer
toInteger (Number x y) = if y < 0 then Nothing else Just $ x * 10 ^ y

fromRational :: Rational -> Number
fromRational r =
    let
        n = Ratio.numerator r
        d1 = Ratio.denominator r
        a :: Integer
        (a, d2) = factor 2 0 d1
        b :: Integer
        (b, d3) = factor 5 0 d2
        e = max a b
    in if d3 == 1
    then normalize $ Number (fromInteger n * 2 ^ (e - a) * 5 ^ (e - b)) (negate e)
    else error $ "fromRational: " <> show r

factor :: (Num a, Integral b) => b -> a -> b -> (a, b)
factor d c n =
    let (q, r) = quotRem n d
    in if n /= 0 && r == 0
    then factor d (c + 1) q
    else (c, n)

get :: TextGet.TextGet Number
get = do
    mn <- TextGet.optional $ TextGet.char '-'
    (nd, ds) <- TextGet.counted getDigit
    Monad.when (nd == 0) $ fail "no integer digits"
    Monad.when (nd > 1 && last ds == 0) $ fail "leading zero"
    (nf, fs) <- fmap (Maybe.fromMaybe (0, [])) . TextGet.optional $ do
        TextGet.char_ '.'
        (nf, fs) <- TextGet.counted getDigit
        Monad.when (nf == 0) $ fail "no fraction digits"
        pure (nf, fs)
    (ms, (_, es)) <- fmap (Maybe.fromMaybe (Nothing, (0, []))) . TextGet.optional $ do
        TextGet.choice $ fmap TextGet.char_ "eE"
        ms <- TextGet.optional . TextGet.satisfy $ \ x -> x == '+' || x == '-'
        (ne, es) <- TextGet.counted getDigit
        Monad.when (ne == 0) $ fail "no exponent digits"
        pure (ms, (ne, es))
    TextGet.spaces
    let
        x = negateIf (mn == Just '-') $ (fromDigits ds * 10 ^ nf) + fromDigits fs
        y = negateIf (ms == Just '-') (fromDigits es) - wordToInteger nf
    pure . normalize $ Number x y

normalize :: Number -> Number
normalize (Number x y) =
    let (q, r) = quotRem x 10
    in if x == 0 then Number x 0
    else if r == 0 then normalize $ Number q (y + 1)
    else Number x y

negateIf :: Num a => Bool -> a -> a
negateIf p x = if p then negate x else x

fromDigits :: [Int] -> Integer
fromDigits = foldr (\ e a -> (a * 10) + intToInteger e) 0

wordToInteger :: Word -> Integer
wordToInteger = fromIntegral

intToInteger :: Int -> Integer
intToInteger = fromIntegral

getDigit :: TextGet.TextGet Int
getDigit = Char.digitToInt <$> TextGet.satisfy Char.isDigit

put :: Number -> TextPut.TextPut
put (Number x y) = TextPut.integer x <> TextPut.char 'e' <> TextPut.integer y
