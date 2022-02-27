module Rattletrap.Type.Attribute.FlaggedByte where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

data FlaggedByte = FlaggedByte
  { flag :: Bool
  , byte :: U8.U8
  }
  deriving (Eq, Show)

instance Argo.HasCodec FlaggedByte where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Forbid
      $ FlaggedByte
      <$> Argo.required flag "flag"
      <*> Argo.required byte "byte"

bitPut :: FlaggedByte -> BitPut.BitPut
bitPut flaggedByteAttribute = BitPut.bool (flag flaggedByteAttribute)
  <> U8.bitPut (byte flaggedByteAttribute)

bitGet :: BitGet.BitGet FlaggedByte
bitGet = BitGet.label "FlaggedByte" $ do
  flag <- BitGet.label "flag" BitGet.bool
  byte <- BitGet.label "byte" U8.bitGet
  pure FlaggedByte { flag, byte }
