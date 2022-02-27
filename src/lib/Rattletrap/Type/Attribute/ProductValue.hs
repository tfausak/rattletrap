module Rattletrap.Type.Attribute.ProductValue where

import qualified Data.Word as Word
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Exception.MissingProductName as MissingProductName
import qualified Rattletrap.Exception.UnknownProduct as UnknownProduct
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Utility.Monad as Monad
import qualified Rattletrap.Vendor.Argo as Argo

data ProductValue
  = PaintedOld CompressedWord.CompressedWord
  | PaintedNew Word.Word32
  | TeamEditionOld CompressedWord.CompressedWord
  | TeamEditionNew Word.Word32
  | SpecialEdition Word.Word32
  | UserColorOld (Maybe Word.Word32)
  | UserColorNew U32.U32
  | TitleId Str.Str
  deriving (Eq, Show)

instance Argo.HasCodec ProductValue where
  codec = Argo.identified $ Argo.oneOf
    [ Argo.mapMaybe
      (Just . PaintedOld)
      (\x -> case x of
        PaintedOld y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "painted_old"
    , Argo.mapMaybe
      (Just . PaintedNew)
      (\x -> case x of
        PaintedNew y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "painted_new"
    , Argo.mapMaybe
      (Just . TeamEditionOld)
      (\x -> case x of
        TeamEditionOld y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "team_edition_old"
    , Argo.mapMaybe
      (Just . TeamEditionNew)
      (\x -> case x of
        TeamEditionNew y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "team_edition_new"
    , Argo.mapMaybe
      (Just . SpecialEdition)
      (\x -> case x of
        SpecialEdition y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "special_edition"
    , Argo.mapMaybe
      (Just . UserColorOld)
      (\x -> case x of
        UserColorOld y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "user_color_old"
    , Argo.mapMaybe
      (Just . UserColorNew)
      (\x -> case x of
        UserColorNew y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "user_color_new"
    , Argo.mapMaybe
      (Just . TitleId)
      (\x -> case x of
        TitleId y -> Just y
        _ -> Nothing
      )
    . Argo.fromObjectCodec Argo.Allow
    $ Argo.required id "title_id"
    ]

bitPut :: ProductValue -> BitPut.BitPut
bitPut val = case val of
  PaintedOld x -> CompressedWord.bitPut x
  PaintedNew x -> BitPut.bits 31 x
  TeamEditionOld x -> CompressedWord.bitPut x
  TeamEditionNew x -> BitPut.bits 31 x
  SpecialEdition x -> BitPut.bits 31 x
  UserColorOld x -> case x of
    Nothing -> BitPut.bool False
    Just y -> BitPut.bool True <> BitPut.bits 31 y
  UserColorNew x -> U32.bitPut x
  TitleId x -> Str.bitPut x

bitGet
  :: Version.Version -> U32.U32 -> Maybe Str.Str -> BitGet.BitGet ProductValue
bitGet version objectId maybeObjectName =
  BitGet.label "ProductValue" $ case fmap Str.toString maybeObjectName of
    Just "TAGame.ProductAttribute_Painted_TA" -> decodePainted version
    Just "TAGame.ProductAttribute_SpecialEdition_TA" -> decodeSpecialEdition
    Just "TAGame.ProductAttribute_TeamEdition_TA" -> decodeTeamEdition version
    Just "TAGame.ProductAttribute_TitleID_TA" -> decodeTitle
    Just "TAGame.ProductAttribute_UserColor_TA" -> decodeColor version
    Just x -> BitGet.throw $ UnknownProduct.UnknownProduct x
    Nothing ->
      BitGet.throw . MissingProductName.MissingProductName $ U32.toWord32
        objectId

decodeSpecialEdition :: BitGet.BitGet ProductValue
decodeSpecialEdition =
  BitGet.label "SpecialEdition" . fmap SpecialEdition $ BitGet.bits 31

decodePainted :: Version.Version -> BitGet.BitGet ProductValue
decodePainted version = BitGet.label "Painted" $ if hasNewPainted version
  then fmap PaintedNew $ BitGet.bits 31
  else fmap PaintedOld $ CompressedWord.bitGet 13

decodeTeamEdition :: Version.Version -> BitGet.BitGet ProductValue
decodeTeamEdition version =
  BitGet.label "TeamEdition" $ if hasNewPainted version
    then fmap TeamEditionNew $ BitGet.bits 31
    else fmap TeamEditionOld $ CompressedWord.bitGet 13

decodeColor :: Version.Version -> BitGet.BitGet ProductValue
decodeColor version =
  BitGet.label "UserColor" $ if Version.atLeast 868 23 8 version
    then fmap UserColorNew U32.bitGet
    else do
      hasValue <- BitGet.bool
      fmap UserColorOld $ Monad.whenMaybe hasValue (BitGet.bits 31)

hasNewPainted :: Version.Version -> Bool
hasNewPainted = Version.atLeast 868 18 0

decodeTitle :: BitGet.BitGet ProductValue
decodeTitle = BitGet.label "Title" $ fmap TitleId Str.bitGet
