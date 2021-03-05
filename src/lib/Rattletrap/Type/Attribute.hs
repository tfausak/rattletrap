module Rattletrap.Type.Attribute where

import qualified Data.Map as Map
import Prelude hiding (id)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.AttributeValue as AttributeValue
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Attribute = Attribute
  { id :: CompressedWord.CompressedWord
  , name :: Str.Str
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , value :: AttributeValue.AttributeValue
  }
  deriving (Eq, Show)

instance Json.FromJSON Attribute where
  parseJSON = Json.withObject "Attribute" $ \object -> do
    id <- Json.required object "id"
    name <- Json.required object "name"
    value <- Json.required object "value"
    pure Attribute { id, name, value }

instance Json.ToJSON Attribute where
  toJSON x = Json.object
    [ Json.pair "id" $ id x
    , Json.pair "name" $ name x
    , Json.pair "value" $ value x
    ]

schema :: Schema.Schema
schema = Schema.named "attribute" $ Schema.object
  [ (Json.pair "id" $ Schema.ref CompressedWord.schema, True)
  , (Json.pair "name" $ Schema.ref Str.schema, True)
  , (Json.pair "value" $ Schema.ref AttributeValue.schema, True)
  ]

bitPut :: Attribute -> BitPut.BitPut
bitPut attribute =
  CompressedWord.bitPut (Rattletrap.Type.Attribute.id attribute)
    <> AttributeValue.bitPut (value attribute)

bitGet
  :: Version.Version
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Attribute
bitGet version classes actors actor = do
  attributes <- lookupAttributeMap classes actors actor
  limit <- lookupAttributeIdLimit attributes actor
  id <- CompressedWord.bitGet limit
  name <- lookupAttributeName classes attributes id
  value <- AttributeValue.bitGet
    version
    (ClassAttributeMap.objectMap classes)
    name
  pure Attribute { id, name, value }

lookupAttributeMap
  :: ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet (Map.Map U32.U32 U32.U32)
lookupAttributeMap classes actors actor = fromMaybe
  ("[RT01] could not get attribute map for " <> show actor)
  (ClassAttributeMap.getAttributeMap classes actors actor)

lookupAttributeIdLimit
  :: Map.Map U32.U32 U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Word
lookupAttributeIdLimit attributes actor = fromMaybe
  ("[RT02] could not get attribute ID limit for " <> show actor)
  (ClassAttributeMap.getAttributeIdLimit attributes)

lookupAttributeName
  :: ClassAttributeMap.ClassAttributeMap
  -> Map.Map U32.U32 U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Str.Str
lookupAttributeName classes attributes attribute = fromMaybe
  ("[RT03] could not get attribute name for " <> show attribute)
  (ClassAttributeMap.getAttributeName classes attributes attribute)

fromMaybe :: String -> Maybe a -> BitGet.BitGet a
fromMaybe message = maybe (fail message) pure
