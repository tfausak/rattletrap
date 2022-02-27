module Rattletrap.Type.RemoteId.PsyNet where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Type.U64 as U64
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data PsyNet
  = New U64.U64
  | Old U64.U64 U64.U64 U64.U64 U64.U64
  deriving (Eq, Show)

instance Argo.HasCodec PsyNet where
  codec =
    Argo.identified
      $ Argo.mapMaybe
          (Just . New)
          (\x -> case x of
            New y -> Just y
            _ -> Nothing
          )
          (Argo.fromObjectCodec Argo.Allow (Argo.required id "Left"))
      Argo.<|> Argo.mapMaybe
                 (\(a, b, c, d) -> Just $ Old a b c d)
                 (\x -> case x of
                   Old a b c d -> Just (a, b, c, d)
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec Argo.Allow (Argo.required id "Right"))

bitPut :: PsyNet -> BitPut.BitPut
bitPut x = case x of
  New l -> U64.bitPut l
  Old a b c d -> U64.bitPut a <> U64.bitPut b <> U64.bitPut c <> U64.bitPut d

bitGet :: Version.Version -> BitGet.BitGet PsyNet
bitGet version = BitGet.label "PsyNet" $ if Version.atLeast 868 24 10 version
  then BitGet.label "New" $ fmap New U64.bitGet
  else BitGet.label "Old" $ do
    a <- U64.bitGet
    b <- U64.bitGet
    c <- U64.bitGet
    d <- U64.bitGet
    pure $ Old a b c d
