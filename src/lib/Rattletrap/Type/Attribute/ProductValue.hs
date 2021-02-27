module Rattletrap.Type.Attribute.ProductValue where

import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import Rattletrap.Utility.Monad
import qualified Rattletrap.Utility.Json as Json
import qualified Rattletrap.Schema as Schema

data ProductValue
  = PaintedOld CompressedWord.CompressedWord
  | PaintedNew Word32
  | TeamEditionOld CompressedWord.CompressedWord
  | TeamEditionNew Word32
  | SpecialEdition Word32
  | UserColorOld (Maybe Word32)
  | UserColorNew U32.U32
  | TitleId Str.Str
  deriving (Eq, Show)

$(deriveJson ''ProductValue)

schema :: Schema.Schema
schema = Schema.named "attribute-product-value"
  . Schema.oneOf
  $ fmap (\ (k, v) -> Schema.object [(Json.pair k v, True)])
  [ ("painted_old", Schema.ref CompressedWord.schema)
  , ("painted_new", Schema.ref Schema.integer)
  , ("team_edition_old", Schema.ref CompressedWord.schema)
  , ("team_edition_new", Schema.ref Schema.integer)
  , ("special_edition", Schema.ref Schema.integer)
  , ("user_color_old", Schema.json $ Schema.maybe Schema.integer)
  , ("user_color_new", Schema.ref U32.schema)
  , ("title_id", Schema.ref Str.schema)
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
  case Str.toString <$> maybeObjectName of
    Just "TAGame.ProductAttribute_Painted_TA" -> decodePainted version
    Just "TAGame.ProductAttribute_SpecialEdition_TA" -> decodeSpecialEdition
    Just "TAGame.ProductAttribute_TeamEdition_TA" -> decodeTeamEdition version
    Just "TAGame.ProductAttribute_TitleID_TA" -> decodeTitle
    Just "TAGame.ProductAttribute_UserColor_TA" -> decodeColor version
    Just objectName -> fail
      ("[RT05] unknown object name "
      <> show objectName
      <> " for ID "
      <> show objectId
      )
    Nothing -> fail ("[RT06] missing object name for ID " <> show objectId)

decodeSpecialEdition :: BitGet.BitGet ProductValue
decodeSpecialEdition = SpecialEdition <$> BitGet.bits 31

decodePainted :: Version.Version -> BitGet.BitGet ProductValue
decodePainted version = if hasNewPainted version
  then PaintedNew <$> BitGet.bits 31
  else PaintedOld <$> CompressedWord.bitGet 13

decodeTeamEdition :: Version.Version -> BitGet.BitGet ProductValue
decodeTeamEdition version = if hasNewPainted version
  then TeamEditionNew <$> BitGet.bits 31
  else TeamEditionOld <$> CompressedWord.bitGet 13

decodeColor :: Version.Version -> BitGet.BitGet ProductValue
decodeColor version = if hasNewColor version
  then UserColorNew <$> U32.bitGet
  else do
    hasValue <- BitGet.bool
    UserColorOld <$> whenMaybe hasValue (BitGet.bits 31)

hasNewPainted :: Version.Version -> Bool
hasNewPainted v =
  Version.major v >= 868 && Version.minor v >= 18 && Version.patch v >= 0

hasNewColor :: Version.Version -> Bool
hasNewColor v =
  Version.major v >= 868 && Version.minor v >= 23 && Version.patch v >= 8

decodeTitle :: BitGet.BitGet ProductValue
decodeTitle = TitleId <$> Str.bitGet
