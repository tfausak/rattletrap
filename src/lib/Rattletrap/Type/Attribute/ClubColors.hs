module Rattletrap.Type.Attribute.ClubColors where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U8 as U8
import qualified Rattletrap.Vendor.Argo as Argo

data ClubColors = ClubColors
  { blueFlag :: Bool
  , blueColor :: U8.U8
  , orangeFlag :: Bool
  , orangeColor :: U8.U8
  }
  deriving (Eq, Show)

instance Argo.HasCodec ClubColors where
  codec =
    Argo.identified
      . Argo.fromObjectCodec Argo.Allow
      $ ClubColors
      <$> Argo.required blueFlag "blue_flag"
      <*> Argo.required blueColor "blue_color"
      <*> Argo.required orangeFlag "orange_flag"
      <*> Argo.required orangeColor "orange_color"

bitPut :: ClubColors -> BitPut.BitPut
bitPut clubColorsAttribute =
  BitPut.bool (blueFlag clubColorsAttribute)
    <> U8.bitPut (blueColor clubColorsAttribute)
    <> BitPut.bool (orangeFlag clubColorsAttribute)
    <> U8.bitPut (orangeColor clubColorsAttribute)

bitGet :: BitGet.BitGet ClubColors
bitGet = BitGet.label "ClubColors" $ do
  blueFlag <- BitGet.label "blueFlag" BitGet.bool
  blueColor <- BitGet.label "blueColor" U8.bitGet
  orangeFlag <- BitGet.label "orangeFlag" BitGet.bool
  orangeColor <- BitGet.label "orangeColor" U8.bitGet
  pure ClubColors { blueFlag, blueColor, orangeFlag, orangeColor }
