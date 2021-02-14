{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ProductValue where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Utility.Monad
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.BitGet as BitGet

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

bitPut :: ProductValue -> BitPut.BitPut
bitPut val = case val of
  PaintedOld x -> CompressedWord.bitPut x
  PaintedNew x -> BitPut.bits 31 x
  TeamEditionOld x -> CompressedWord.bitPut x
  TeamEditionNew x -> BitPut.bits 31 x
  SpecialEdition x -> BitPut.bits 31 x
  UserColorOld x -> case x of
    Nothing -> BitPut.bool False
    Just y ->
      BitPut.bool True
      <> BitPut.bits 31 y
  UserColorNew x -> U32.bitPut x
  TitleId x -> Str.bitPut x

bitGet
  :: (Int, Int, Int) -> U32.U32 -> Maybe Str.Str -> BitGet.BitGet ProductValue
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

decodePainted :: (Int, Int, Int) -> BitGet.BitGet ProductValue
decodePainted version = if version >= (868, 18, 0)
  then PaintedNew <$> BitGet.bits 31
  else PaintedOld <$> CompressedWord.bitGet 13

decodeTeamEdition :: (Int, Int, Int) -> BitGet.BitGet ProductValue
decodeTeamEdition version = if version >= (868, 18, 0)
  then TeamEditionNew <$> BitGet.bits 31
  else TeamEditionOld <$> CompressedWord.bitGet 13

decodeColor :: (Int, Int, Int) -> BitGet.BitGet ProductValue
decodeColor version = if version >= (868, 23, 8)
  then UserColorNew <$> U32.bitGet
  else do
    hasValue <- BitGet.bool
    UserColorOld <$> whenMaybe hasValue (BitGet.bits 31)

decodeTitle :: BitGet.BitGet ProductValue
decodeTitle = TitleId <$> Str.bitGet
