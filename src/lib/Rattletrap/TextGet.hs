module Rattletrap.TextGet where

import qualified Control.Applicative as Applicative
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Array as Array
import qualified Data.Functor.Identity as Identity
import qualified Data.Text as Text
import qualified Rattletrap.Exception.NotEnoughInput as NotEnoughInput
import qualified Rattletrap.Get as Get

type TextGet = Get.Get Text.Text Identity.Identity

run
    :: TextGet a
    -> Text.Text
    -> Either ([String], Exception.SomeException) a
run g = fmap snd . Identity.runIdentity . Get.run g

array :: TextGet l -> TextGet s -> TextGet r -> TextGet a -> TextGet (Array.Array Word a)
array l s r f = l *> arrayHelper s f 0 [] <* r

arrayHelper :: TextGet s -> TextGet a -> Word -> [(Word, a)] -> TextGet (Array.Array Word a)
arrayHelper s f i xs = do
    m <- optional $ do
        Monad.when (i /= 0) $ Monad.void s
        f
    case m of
        Nothing -> pure $ Array.array (if i == 0 then (1, 0) else (0, i - 1)) xs
        Just x -> arrayHelper s f (i + 1) $ (i, x) : xs

char :: Char -> TextGet Char
char = satisfy . (==)

char_ :: Char -> TextGet ()
char_ = Monad.void . char

choice :: [TextGet a] -> TextGet a
choice = foldr (Applicative.<|>) Applicative.empty

count :: Int -> TextGet a -> TextGet [a]
count = Monad.replicateM

counted :: TextGet a -> TextGet (Word, [a]) -- reversed!
counted = countedWith 0 []

countedWith :: Word -> [a] -> TextGet a -> TextGet (Word, [a])
countedWith i xs f = do
    m <- optional f
    case m of
        Nothing -> pure (i, xs)
        Just x -> countedWith (i + 1) (x : xs) f

eof :: TextGet ()
eof = do
    s <- Get.get
    Monad.unless (Text.null s) $ fail "eof"

isSpace :: Char -> Bool
isSpace x = case x of
    ' ' -> True
    '\n' -> True
    '\r' -> True
    '\t' -> True
    _ -> False

many :: TextGet a -> TextGet [a]
many = Applicative.many

optional :: TextGet a -> TextGet (Maybe a)
optional = Applicative.optional

satisfy :: (Char -> Bool) -> TextGet Char
satisfy f = do
    s1 <- Get.get
    case Text.uncons s1 of
        Nothing -> Get.throw NotEnoughInput.NotEnoughInput
        Just (x, s2)
            | f x -> do
                Get.put s2
                pure x
            | otherwise -> fail $ "satisfy: " <> show x

skipWhile :: (Char -> Bool) -> TextGet ()
skipWhile f = do
    s1 <- Get.get
    Get.put $ Text.dropWhile f s1

spaces :: TextGet ()
spaces = skipWhile isSpace

string :: String -> TextGet String
string = fmap Text.unpack . text . Text.pack

text :: Text.Text -> TextGet Text.Text
text e = do
    s1 <- Get.get
    case Text.stripPrefix e s1 of
        Nothing -> fail $ "text: " <> show e
        Just s2 -> do
            Get.put s2
            pure e
