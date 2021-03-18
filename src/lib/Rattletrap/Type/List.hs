module Rattletrap.Type.List where

import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Utility.Json as Json

import qualified Control.Monad as Monad

newtype List a
  = List [a]
  deriving (Eq, Show)

instance Json.FromJSON a => Json.FromJSON (List a) where
  parseJSON = fmap fromList . Json.parseJSON

instance Json.ToJSON a => Json.ToJSON (List a) where
  toJSON = Json.toJSON . toList

schema :: Schema.Schema -> Schema.Schema
schema = Schema.array

fromList :: [a] -> List a
fromList = List

empty :: List a
empty = fromList []

toList :: List a -> [a]
toList (List x) = x

bytePut :: (a -> BytePut.BytePut) -> List a -> BytePut.BytePut
bytePut f x =
  let v = toList x
  in (U32.bytePut . U32.fromWord32 . fromIntegral $ length v) <> foldMap f v

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (List a)
byteGet f = ByteGet.label "List" $ do
  size <- ByteGet.label "size" U32.byteGet
  generateM (fromIntegral $ U32.toWord32 size)
    $ \i -> ByteGet.label ("element (" <> show i <> ")") f

generateM :: Monad m => Int -> (Int -> m a) -> m (List a)
generateM n f = fmap fromList $ mapM f [0 .. n - 1]

replicateM :: Monad m => Int -> m a -> m (List a)
replicateM n = fmap fromList . Monad.replicateM n

untilM :: Monad m => m (Maybe a) -> m (List a)
untilM f = untilMWith f 0 []

untilMWith :: Monad m => m (Maybe a) -> Int -> [(Int, a)] -> m (List a)
untilMWith f i xs = do
  m <- f
  case m of
    Nothing -> pure . fromList . reverse $ fmap snd xs
    Just x -> untilMWith f (i + 1) ((i, x) : xs)
