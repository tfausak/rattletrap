module Rattletrap.Type.List where

import Rattletrap.Decode.Common
import Rattletrap.Encode.Common
import qualified Rattletrap.Type.U32 as U32

import qualified Data.Aeson as Aeson
import qualified Data.Array as Array
import qualified Data.Foldable as Foldable

newtype List a
  = List (Array.Array Int a)
  deriving (Eq, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (List a) where
  parseJSON = Aeson.withArray "List" $ \ xs -> do
    ys <- traverse Aeson.parseJSON xs
    pure . fromArray . Array.listArray (0, length ys - 1) $ Foldable.toList ys

instance Aeson.ToJSON a => Aeson.ToJSON (List a) where
  toJSON = Aeson.toJSON . toList

fromArray :: Array.Array Int a -> List a
fromArray = List

empty :: List a
empty = fromArray $ Array.listArray (0, -1) []

toArray :: List a -> Array.Array Int a
toArray (List x) = x

toList :: List a -> [a]
toList = Array.elems . toArray

bytePut :: (a -> BytePut) -> List a -> BytePut
bytePut f x = do
  U32.bytePut . U32.fromWord32 . fromIntegral . length $ toArray x
  mapM_ f $ toArray x

byteGet :: ByteGet a -> ByteGet (List a)
byteGet f = do
  size <- U32.byteGet
  replicateM (fromIntegral $ U32.toWord32 size) f

replicateM :: Monad m => Int -> m a -> m (List a)
replicateM n = generateM n . const

generateM :: Monad m => Int -> (Int -> m a) -> m (List a)
generateM = generateMWith 0 []

generateMWith
  :: Monad m => Int -> [(Int, a)] -> Int -> (Int -> m a) -> m (List a)
generateMWith i xs n f = if i >= n
  then pure . fromArray $ Array.array (0, n - 1) xs
  else do
    x <- f i
    generateMWith (i + 1) ((i, x) : xs) n f

untilM :: Monad m => m (Maybe a) -> m (List a)
untilM = untilMWith 0 []

untilMWith :: Monad m => Int -> [(Int, a)] -> m (Maybe a) -> m (List a)
untilMWith i xs f = do
  m <- f
  case m of
    Nothing -> pure . fromArray $ Array.array (0, i - 1) xs
    Just x -> untilMWith (i + 1) ((i, x) : xs) f
