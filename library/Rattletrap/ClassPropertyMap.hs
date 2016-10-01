module Rattletrap.ClassPropertyMap where

import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.List
import Rattletrap.Text

data ClassPropertyMap = ClassPropertyMap
  {
  } deriving (Eq, Ord, Show)

makeClassPropertyMap :: List Text
                     -> List ClassMapping
                     -> List Cache
                     -> ClassPropertyMap
makeClassPropertyMap _objects _classMappings _caches = ClassPropertyMap {}
