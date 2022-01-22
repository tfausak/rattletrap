module Rattletrap.Type.Replication.Updated where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Attribute as Attribute
import qualified Rattletrap.Type.ClassAttributeMap as ClassAttributeMap
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.List as List
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

newtype Updated = Updated
  { attributes :: List.List Attribute.Attribute
  } deriving (Eq, Show)

instance Argo.HasCodec Updated where
  codec = Argo.map Updated attributes Argo.codec

bitPut :: Updated -> BitPut.BitPut
bitPut x =
  foldMap
      (\y -> BitPut.bool True <> Attribute.bitPut y)
      (List.toList $ attributes x)
    <> BitPut.bool False

bitGet
  :: Version.Version
  -> ClassAttributeMap.ClassAttributeMap
  -> Map.Map CompressedWord.CompressedWord U32.U32
  -> CompressedWord.CompressedWord
  -> BitGet.BitGet Updated
bitGet version classes actors actor =
  BitGet.label "Updated" . fmap Updated . List.untilM $ do
    p <- BitGet.bool
    Monad.whenMaybe p $ Attribute.bitGet version classes actors actor
