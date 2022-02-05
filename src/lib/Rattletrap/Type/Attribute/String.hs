module Rattletrap.Type.Attribute.String where

import Prelude hiding (String)
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Vendor.Argo as Argo

newtype String = String
  { value :: Str.Str
  } deriving (Eq, Show)

instance Argo.HasCodec String where
  codec = Argo.identified $ Argo.map String value Argo.codec

bitPut :: String -> BitPut.BitPut
bitPut stringAttribute = Str.bitPut (value stringAttribute)

bitGet :: BitGet.BitGet String
bitGet = BitGet.label "String" $ do
  value <- BitGet.label "value" Str.bitGet
  pure String { value }
