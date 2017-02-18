module Rattletrap.Map.Object
  ( ObjectMap
  , makeObjectMap
  , objectMapLookup
  , objectMapKeys
  ) where

import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Word as Word

newtype ObjectMap =
  ObjectMap (HashMap.HashMap Word.Word32 Text)
  deriving (Eq, Show)

makeObjectMap :: List Text -> ObjectMap
makeObjectMap objects =
  ObjectMap
    (Vector.ifoldr
       (\i -> HashMap.insert (fromIntegral i))
       HashMap.empty
       (listValue objects))

objectMapLookup :: Word32 -> ObjectMap -> Maybe Text
objectMapLookup objectId (ObjectMap objectMap) =
  HashMap.lookup (word32Value objectId) objectMap

objectMapKeys :: ObjectMap -> [Word32]
objectMapKeys (ObjectMap objectMap) = map Word32 (HashMap.keys objectMap)
