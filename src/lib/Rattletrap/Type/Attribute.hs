module Rattletrap.Type.Attribute where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.AttributeValue as AttributeValue
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32

data Attribute = Attribute
  { id :: CompressedWord.CompressedWord
  , name :: Str.Str
  -- ^ Read-only! Changing an attribute's name requires editing the class
  -- attribute map.
  , value :: AttributeValue.AttributeValue
  }
  deriving (Eq, Show)

$(deriveJson ''Attribute)

bitPut :: Attribute -> BitPut.BitPut
bitPut attribute =
  CompressedWord.bitPut (Rattletrap.Type.Attribute.id attribute)
    <> AttributeValue.bitPut (value attribute)

bitGet
  :: (Int, Int, Int)
  -> ClassAttributeMap.ClassAttributeMap
  -> Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Attribute
bitGet version classes actors actor = do
  attributes <- lookupAttributeMap classes actors actor
  limit <- lookupAttributeIdLimit attributes actor
  attribute <- CompressedWord.bitGet limit
  name_ <- lookupAttributeName classes attributes attribute
  Attribute attribute name_
    <$> AttributeValue.bitGet
          version
          (ClassAttributeMap.objectMap classes)
          name_

lookupAttributeMap
  :: ClassAttributeMap.ClassAttributeMap
  -> Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet (Map U32.U32 U32.U32)
lookupAttributeMap classes actors actor = fromMaybe
  ("[RT01] could not get attribute map for " <> show actor)
  (ClassAttributeMap.getAttributeMap classes actors actor)

lookupAttributeIdLimit
  :: Map U32.U32 U32.U32 -> CompressedWord.CompressedWord -> BitGet.BitGet Word
lookupAttributeIdLimit attributes actor = fromMaybe
  ("[RT02] could not get attribute ID limit for " <> show actor)
  (ClassAttributeMap.getAttributeIdLimit attributes)

lookupAttributeName
  :: ClassAttributeMap.ClassAttributeMap
  -> Map U32.U32 U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Str.Str
lookupAttributeName classes attributes attribute = fromMaybe
  ("[RT03] could not get attribute name for " <> show attribute)
  (ClassAttributeMap.getAttributeName classes attributes attribute)

fromMaybe :: String -> Maybe a -> BitGet.BitGet a
fromMaybe message = maybe (fail message) pure
