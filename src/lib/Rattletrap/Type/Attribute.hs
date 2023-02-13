module Rattletrap.Type.Attribute where

import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.MissingAttributeLimit as MissingAttributeLimit
import qualified Rattletrap.Exception.MissingAttributeName as MissingAttributeName
import qualified Rattletrap.Exception.UnknownActor as UnknownActor
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.AttributeValue as AttributeValue
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json

data Attribute = Attribute
  { id :: CompressedWord.CompressedWord,
    -- | Read-only! Changing an attribute's name requires editing the class
    -- attribute map.
    name :: Str.Str,
    value :: AttributeValue.AttributeValue
  }
  deriving (Eq, Show)

instance Json.FromJSON Attribute where
  parseJSON = Json.withObject "Attribute" $ \object -> do
    id_ <- Json.required object "id"
    name <- Json.required object "name"
    value <- Json.required object "value"
    pure Attribute {Rattletrap.Type.Attribute.id = id_, name, value}

instance Json.ToJSON Attribute where
  toJSON x =
    Json.object
      [ Json.pair "id" $ Rattletrap.Type.Attribute.id x,
        Json.pair "name" $ name x,
        Json.pair "value" $ value x
      ]

schema :: Schema.Schema
schema =
  Schema.named "attribute" $
    Schema.object
      [ (Json.pair "id" $ Schema.ref CompressedWord.schema, True),
        (Json.pair "name" $ Schema.ref Str.schema, True),
        (Json.pair "value" $ Schema.ref AttributeValue.schema, True)
      ]

bitPut :: Attribute -> BitPut.BitPut
bitPut attribute =
  CompressedWord.bitPut (Rattletrap.Type.Attribute.id attribute)
    <> AttributeValue.bitPut (value attribute)

bitGet ::
  Version.Version ->
  Maybe Str.Str ->
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  CompressedWord.CompressedWord ->
  BitGet.BitGet Attribute
bitGet version buildVersion classes actors actor =
  BitGet.label "Attribute" $ do
    attributes <- lookupAttributeMap classes actors actor
    limit <- lookupAttributeIdLimit attributes actor
    id_ <- BitGet.label "id" $ CompressedWord.bitGet limit
    name <- lookupAttributeName classes attributes id_
    value <-
      BitGet.label "value" $
        AttributeValue.bitGet
          version
          buildVersion
          (ClassAttributeMap.objectMap classes)
          name
    pure Attribute {Rattletrap.Type.Attribute.id = id_, name, value}

lookupAttributeMap ::
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  CompressedWord.CompressedWord ->
  BitGet.BitGet (Map.Map U32.U32 U32.U32)
lookupAttributeMap classes actors actor =
  fromMaybe
    (UnknownActor.UnknownActor $ CompressedWord.value actor)
    (ClassAttributeMap.getAttributeMap classes actors actor)

lookupAttributeIdLimit ::
  Map.Map U32.U32 U32.U32 ->
  CompressedWord.CompressedWord ->
  BitGet.BitGet Word
lookupAttributeIdLimit attributes actor =
  fromMaybe
    (MissingAttributeLimit.MissingAttributeLimit $ CompressedWord.value actor)
    (ClassAttributeMap.getAttributeIdLimit attributes)

lookupAttributeName ::
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map U32.U32 U32.U32 ->
  CompressedWord.CompressedWord ->
  BitGet.BitGet Str.Str
lookupAttributeName classes attributes attribute =
  fromMaybe
    (MissingAttributeName.MissingAttributeName $ CompressedWord.value attribute)
    (ClassAttributeMap.getAttributeName classes attributes attribute)

fromMaybe :: (Exception.Exception e) => e -> Maybe a -> BitGet.BitGet a
fromMaybe message = maybe (BitGet.throw message) pure
