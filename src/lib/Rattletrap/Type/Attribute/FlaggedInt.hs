module Rattletrap.Type.Attribute.FlaggedInt where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.I32 as I32
import qualified Rattletrap.Vendor.Argo as Argo

data FlaggedInt = FlaggedInt
  { flag :: Bool
  , int :: I32.I32
  }
  deriving (Eq, Show)

instance Argo.HasCodec FlaggedInt where
  codec =
    Argo.fromObjectCodec Argo.Allow
      $ FlaggedInt
      <$> Argo.project flag (Argo.required (Argo.fromString "flag") Argo.codec)
      <*> Argo.project int (Argo.required (Argo.fromString "int") Argo.codec)

bitPut :: FlaggedInt -> BitPut.BitPut
bitPut flaggedIntAttribute = BitPut.bool (flag flaggedIntAttribute)
  <> I32.bitPut (int flaggedIntAttribute)

bitGet :: BitGet.BitGet FlaggedInt
bitGet = BitGet.label "FlaggedInt" $ do
  flag <- BitGet.label "flag" BitGet.bool
  int <- BitGet.label "int" I32.bitGet
  pure FlaggedInt { flag, int }
