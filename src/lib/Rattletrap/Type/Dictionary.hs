module Rattletrap.Type.Dictionary where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

data Dictionary a = Dictionary
  { elements :: List.List (Str.Str, a)
  , lastKey :: Str.Str
  }
  deriving (Eq, Show)

instance (Argo.HasCodec a, Typeable.Typeable a) => Argo.HasCodec (Dictionary a) where
  codec =
    Argo.identified
      . Argo.mapMaybe
          (\(keys, lastKey, value) ->
            Dictionary
              <$> fmap
                    List.fromList
                    (mapM
                      (\k -> fmap ((,) (Str.fromText k)) $ Map.lookup k value)
                      keys
                    )
              <*> pure lastKey
          )
          (\x -> Just
            ( fmap (Str.toText . fst) . List.toList $ elements x
            , lastKey x
            , Map.mapKeys Str.toText . Map.fromList . List.toList $ elements x
            )
          )
      . Argo.fromObjectCodec Argo.Allow
      $ (,,)
      <$> Argo.project
            (\(x, _, _) -> x)
            (Argo.required (Argo.fromString "keys") Argo.codec)
      <*> Argo.project
            (\(_, x, _) -> x)
            (Argo.required (Argo.fromString "last_key") Argo.codec)
      <*> Argo.project
            (\(_, _, x) -> x)
            (Argo.required (Argo.fromString "value") Argo.codec)

lookup :: Str.Str -> Dictionary a -> Maybe a
lookup k = Prelude.lookup k . List.toList . elements

bytePut :: (a -> BytePut.BytePut) -> Dictionary a -> BytePut.BytePut
bytePut f x =
  foldMap (\(k, v) -> Str.bytePut k <> f v) (List.toList $ elements x)
    <> Str.bytePut (lastKey x)

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Dictionary a)
byteGet = ByteGet.label "Dictionary" . byteGetWith 0 []

byteGetWith
  :: Int
  -> [(Int, (Str.Str, a))]
  -> ByteGet.ByteGet a
  -> ByteGet.ByteGet (Dictionary a)
byteGetWith i xs f = do
  k <- ByteGet.label ("key (" <> show i <> ")") Str.byteGet
  if isNone k
    then pure Dictionary
      { elements = List.fromList . reverse $ fmap snd xs
      , lastKey = k
      }
    else do
      v <- ByteGet.label ("value (" <> Str.toString k <> ")") f
      byteGetWith (i + 1) ((i, (k, v)) : xs) f

isNone :: Str.Str -> Bool
isNone = (== Text.pack "None") . Text.filter (/= '\x00') . Str.toText
