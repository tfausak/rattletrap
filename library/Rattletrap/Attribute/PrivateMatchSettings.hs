module Rattletrap.Attribute.PrivateMatchSettings where

import Rattletrap.Primitive

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data PrivateMatchSettingsAttribute = PrivateMatchSettingsAttribute
  { privateMatchSettingsAttributeMutators :: Text
  , privateMatchSettingsAttributeJoinableBy :: Word32
  , privateMatchSettingsAttributeMaxPlayers :: Word32
  , privateMatchSettingsAttributeGameName :: Text
  , privateMatchSettingsAttributePassword :: Text
  , privateMatchSettingsAttributeFlag :: Bool
  } deriving (Eq, Ord, Show)

getPrivateMatchSettingsAttribute :: BinaryBit.BitGet PrivateMatchSettingsAttribute
getPrivateMatchSettingsAttribute = do
  mutators <- getTextBits
  joinableBy <- getWord32Bits
  maxPlayers <- getWord32Bits
  gameName <- getTextBits
  password <- getTextBits
  flag <- BinaryBit.getBool
  pure
    (PrivateMatchSettingsAttribute
       mutators
       joinableBy
       maxPlayers
       gameName
       password
       flag)

putPrivateMatchSettingsAttribute :: PrivateMatchSettingsAttribute
                                      -> BinaryBit.BitPut ()
putPrivateMatchSettingsAttribute privateMatchSettingsAttribute = do
  putTextBits
    (privateMatchSettingsAttributeMutators
       privateMatchSettingsAttribute)
  putWord32Bits
    (privateMatchSettingsAttributeJoinableBy
       privateMatchSettingsAttribute)
  putWord32Bits
    (privateMatchSettingsAttributeMaxPlayers
       privateMatchSettingsAttribute)
  putTextBits
    (privateMatchSettingsAttributeGameName
       privateMatchSettingsAttribute)
  putTextBits
    (privateMatchSettingsAttributePassword
       privateMatchSettingsAttribute)
  BinaryBit.putBool
    (privateMatchSettingsAttributeFlag privateMatchSettingsAttribute)
