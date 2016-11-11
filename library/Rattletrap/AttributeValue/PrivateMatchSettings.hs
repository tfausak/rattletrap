module Rattletrap.AttributeValue.PrivateMatchSettings where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PrivateMatchSettingsAttributeValue = PrivateMatchSettingsAttributeValue
  { privateMatchSettingsAttributeValueMutators :: Text
  , privateMatchSettingsAttributeValueJoinableBy :: Word32
  , privateMatchSettingsAttributeValueMaxPlayers :: Word32
  , privateMatchSettingsAttributeValueGameName :: Text
  , privateMatchSettingsAttributeValuePassword :: Text
  , privateMatchSettingsAttributeValueFlag :: Bool
  } deriving (Eq, Ord, Show)

getPrivateMatchSettingsAttributeValue :: BinaryBit.BitGet PrivateMatchSettingsAttributeValue
getPrivateMatchSettingsAttributeValue = do
  mutators <- getTextBits
  joinableBy <- getWord32Bits
  maxPlayers <- getWord32Bits
  gameName <- getTextBits
  password <- getTextBits
  flag <- BinaryBit.getBool
  pure
    (PrivateMatchSettingsAttributeValue
       mutators
       joinableBy
       maxPlayers
       gameName
       password
       flag)

putPrivateMatchSettingsAttributeValue :: PrivateMatchSettingsAttributeValue
                                      -> BinaryBit.BitPut ()
putPrivateMatchSettingsAttributeValue privateMatchSettingsAttributeValue = do
  putTextBits
    (privateMatchSettingsAttributeValueMutators
       privateMatchSettingsAttributeValue)
  putWord32Bits
    (privateMatchSettingsAttributeValueJoinableBy
       privateMatchSettingsAttributeValue)
  putWord32Bits
    (privateMatchSettingsAttributeValueMaxPlayers
       privateMatchSettingsAttributeValue)
  putTextBits
    (privateMatchSettingsAttributeValueGameName
       privateMatchSettingsAttributeValue)
  putTextBits
    (privateMatchSettingsAttributeValuePassword
       privateMatchSettingsAttributeValue)
  BinaryBit.putBool
    (privateMatchSettingsAttributeValueFlag privateMatchSettingsAttributeValue)
