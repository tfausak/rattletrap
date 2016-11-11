module Rattletrap.Header where

import Rattletrap.Primitive.Dictionary
import Rattletrap.Int32
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Binary as Binary

data Header = Header
  { headerEngineVersion :: Word32
  , headerLicenseeVersion :: Word32
  , headerLabel :: Text
  , headerProperties :: Dictionary Property
  } deriving (Eq, Ord, Show)

getHeader :: Binary.Get Header
getHeader = do
  engineVersion <- getWord32
  licenseeVersion <- getWord32
  label <- getText
  properties <- getDictionary getProperty
  pure (Header engineVersion licenseeVersion label properties)

putHeader :: Header -> Binary.Put
putHeader header = do
  putWord32 (headerEngineVersion header)
  putWord32 (headerLicenseeVersion header)
  putText (headerLabel header)
  putDictionary putProperty (headerProperties header)

getVersion
  :: (Integral a, Integral b)
  => Header -> (a, b)
getVersion header =
  let major = getMajorVersion header
      minor = getMinorVersion header
  in (major, minor)

getMajorVersion
  :: (Integral a)
  => Header -> a
getMajorVersion header = fromIntegral (word32Value (headerEngineVersion header))

getMinorVersion
  :: (Integral a)
  => Header -> a
getMinorVersion header =
  fromIntegral (word32Value (headerLicenseeVersion header))

getNumFrames
  :: (Integral a)
  => Header -> a
getNumFrames header =
  let key = stringToText "NumFrames"
      properties = dictionaryValue (headerProperties header)
  in case lookup key properties of
       Just (Just (Property _ _ (IntProperty numFrames))) ->
         fromIntegral (int32Value numFrames)
       _ -> 0
