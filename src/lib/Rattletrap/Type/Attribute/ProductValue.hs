{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Attribute.ProductValue where

import Rattletrap.Type.Common
import qualified Rattletrap.Type.CompressedWord as CompressedWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import Rattletrap.Encode.Common
import Rattletrap.Decode.Common

import qualified Data.Binary.Bits.Put as BinaryBits

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

bitPut :: ProductValue -> BitPut ()
bitPut val = case val of
  PaintedOld x -> CompressedWord.bitPut x
  PaintedNew x -> putBitsLE 31 x
  TeamEditionOld x -> CompressedWord.bitPut x
  TeamEditionNew x -> putBitsLE 31 x
  SpecialEdition x -> putBitsLE 31 x
  UserColorOld x -> case x of
    Nothing -> BinaryBits.putBool False
    Just y -> do
      BinaryBits.putBool True
      putBitsLE 31 y
  UserColorNew x -> U32.bitPut x
  TitleId x -> Str.bitPut x

bitGet
  :: (Int, Int, Int) -> U32.U32 -> Maybe Str.Str -> BitGet ProductValue
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

decodeSpecialEdition :: BitGet ProductValue
decodeSpecialEdition = SpecialEdition <$> getBitsLE 31

decodePainted :: (Int, Int, Int) -> BitGet ProductValue
decodePainted version = if version >= (868, 18, 0)
  then PaintedNew <$> getBitsLE 31
  else PaintedOld <$> CompressedWord.bitGet 13

decodeTeamEdition :: (Int, Int, Int) -> BitGet ProductValue
decodeTeamEdition version = if version >= (868, 18, 0)
  then TeamEditionNew <$> getBitsLE 31
  else TeamEditionOld <$> CompressedWord.bitGet 13

decodeColor :: (Int, Int, Int) -> BitGet ProductValue
decodeColor version = if version >= (868, 23, 8)
  then UserColorNew <$> U32.bitGet
  else do
    hasValue <- getBool
    UserColorOld <$> decodeWhen hasValue (getBitsLE 31)

decodeTitle :: BitGet ProductValue
decodeTitle = TitleId <$> Str.bitGet
