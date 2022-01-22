module Rattletrap.Type.Attribute where

import qualified Control.Exception as Exception
import qualified Data.Map as Map
import Prelude hiding (id)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.MissingAttributeLimit as MissingAttributeLimit
import qualified Rattletrap.Exception.MissingAttributeName as MissingAttributeName
import qualified Rattletrap.Exception.UnknownActor as UnknownActor
import qualified Rattletrap.Type.AttributeValue as AttributeValue
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data Attribute = Attribute
  { id :: CompressedWord.CompressedWord
  , name :: Str.Str
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , value :: AttributeValue.AttributeValue
  }
  deriving (Eq, Show)

instance Argo.HasCodec Attribute where
  codec = Argo.fromObjectCodec Argo.Allow $ Attribute
    <$> Argo.project id (Argo.required (Argo.fromString "id") Argo.codec)
    <*> Argo.project name (Argo.required (Argo.fromString "name") Argo.codec)
    <*> Argo.project value (Argo.required (Argo.fromString "value") Argo.codec)

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
bitGet version classes actors actor = BitGet.label "Attribute" $ do
  attributes <- lookupAttributeMap classes actors actor
  limit <- lookupAttributeIdLimit attributes actor
  id <- BitGet.label "id" $ CompressedWord.bitGet limit
  name <- lookupAttributeName classes attributes id
  value <- BitGet.label "value" $ AttributeValue.bitGet
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
  (UnknownActor.UnknownActor $ CompressedWord.value actor)
  (ClassAttributeMap.getAttributeMap classes actors actor)

lookupAttributeIdLimit
  :: Map.Map U32.U32 U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Word
lookupAttributeIdLimit attributes actor = fromMaybe
  (MissingAttributeLimit.MissingAttributeLimit $ CompressedWord.value actor)
  (ClassAttributeMap.getAttributeIdLimit attributes)

lookupAttributeName
  :: ClassAttributeMap.ClassAttributeMap
  -> Map.Map U32.U32 U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Str.Str
lookupAttributeName classes attributes attribute = fromMaybe
  (MissingAttributeName.MissingAttributeName $ CompressedWord.value attribute)
  (ClassAttributeMap.getAttributeName classes attributes attribute)

fromMaybe :: Exception.Exception e => e -> Maybe a -> BitGet.BitGet a
fromMaybe message = maybe (BitGet.throw message) pure
