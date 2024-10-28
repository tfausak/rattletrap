module Rattletrap.Type.Dictionary where

import qualified Data.Text as Text
import qualified Rattletrap.ByteGet as ByteGet
import qualified Rattletrap.BytePut as BytePut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.List as RList
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

data Dictionary a = Dictionary
  { elements :: RList.List (Str.Str, a),
    lastKey :: Str.Str
  }
  deriving (Eq, Show)

instance (Json.FromJSON a) => Json.FromJSON (Dictionary a) where
  parseJSON = Json.withObject "Dictionary" $ \o -> do
    elements <- Json.required o "elements"
    lastKey <- Json.required o "last_key"
    pure Dictionary {elements = elements, lastKey = lastKey}

instance (Json.ToJSON a) => Json.ToJSON (Dictionary a) where
  toJSON x =
    Json.object
      [ Json.pair "elements" . RList.toList $ elements x,
        Json.pair "last_key" $ lastKey x
      ]

schema :: Schema.Schema -> Schema.Schema
schema s =
  let name = "dictionary-" <> Text.unpack (Schema.name s)
   in Schema.named name $
        Schema.object
          [ ( Json.pair "elements"
                . Schema.json
                . Schema.array
                . Schema.named (name <> "-element")
                $ Schema.tuple [Schema.ref Str.schema, Schema.ref s],
              True
            ),
            (Json.pair "last_key" $ Schema.ref Str.schema, True)
          ]

lookup :: Str.Str -> Dictionary a -> Maybe a
lookup k = Prelude.lookup k . RList.toList . elements

bytePut :: (a -> BytePut.BytePut) -> Dictionary a -> BytePut.BytePut
bytePut f x =
  foldMap (\(k, v) -> Str.bytePut k <> f v) (RList.toList $ elements x)
    <> Str.bytePut (lastKey x)

byteGet :: ByteGet.ByteGet a -> ByteGet.ByteGet (Dictionary a)
byteGet = ByteGet.label "Dictionary" . byteGetWith 0 []

byteGetWith ::
  Int ->
  [(Int, (Str.Str, a))] ->
  ByteGet.ByteGet a ->
  ByteGet.ByteGet (Dictionary a)
byteGetWith i xs f = do
  k <- ByteGet.label ("key (" <> show i <> ")") Str.byteGet
  if isNone k
    then
      pure
        Dictionary
          { elements = RList.fromList . reverse $ fmap snd xs,
            lastKey = k
          }
    else do
      v <- ByteGet.label ("value (" <> Str.toString k <> ")") f
      byteGetWith (i + 1) ((i, (k, v)) : xs) f

isNone :: Str.Str -> Bool
isNone = (== Text.pack "None") . Text.filter (/= '\x00') . Str.toText
