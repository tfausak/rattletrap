module Rattletrap.Type.Replication.Updated where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute as Attribute
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as RList
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Utility.Monad as Monad

newtype Updated = Updated
  { attributes :: RList.List Attribute.Attribute
  }
  deriving (Eq, Show)

instance Json.FromJSON Updated where
  parseJSON = fmap Updated . Json.parseJSON

instance Json.ToJSON Updated where
  toJSON = Json.toJSON . attributes

schema :: Schema.Schema
schema =
  Schema.named "replication-updated" . Schema.json $
    RList.schema
      Attribute.schema

bitPut :: Updated -> BitPut.BitPut
bitPut x =
  foldMap
    (\y -> BitPut.bool True <> Attribute.bitPut y)
    (RList.toList $ attributes x)
    <> BitPut.bool False

bitGet ::
  Version.Version ->
  Maybe Str.Str ->
  ClassAttributeMap.ClassAttributeMap ->
  Map.Map CompressedWord.CompressedWord U32.U32 ->
  CompressedWord.CompressedWord ->
  BitGet.BitGet Updated
bitGet version buildVersion classes actors actor =
  BitGet.label "Updated" . fmap Updated . RList.untilM $ do
    p <- BitGet.bool
    Monad.whenMaybe p $
      Attribute.bitGet version buildVersion classes actors actor
