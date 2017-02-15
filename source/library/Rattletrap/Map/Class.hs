module Rattletrap.Map.Class
  ( ClassMap
  , makeClassMap
  , classMapLookup
  , classMapLookupR
  ) where

import Rattletrap.ClassMapping
import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Word as Word

data ClassMap = ClassMap
  { classMapNames :: HashMap.HashMap Word.Word32 Text.Text
  , classMapIds :: HashMap.HashMap Text.Text Word.Word32
  } deriving (Eq, Show)

makeClassMap :: List ClassMapping -> ClassMap
makeClassMap classMappings =
  let toElement classMapping =
        ( word32Value (classMappingStreamId classMapping)
        , textValue (classMappingName classMapping))
      elements = map toElement (listValue classMappings)
  in ClassMap
     { classMapNames = HashMap.fromList elements
     , classMapIds = HashMap.fromList (map Tuple.swap elements)
     }

classMapLookup :: Word32 -> ClassMap -> Maybe Text
classMapLookup classId classMap = do
  className <- HashMap.lookup (word32Value classId) (classMapNames classMap)
  pure (Text className)

classMapLookupR :: Text -> ClassMap -> Maybe Word32
classMapLookupR className classMap = do
  classId <- HashMap.lookup (textValue className) (classMapIds classMap)
  pure (Word32 classId)
