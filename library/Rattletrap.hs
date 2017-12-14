-- | Rattletrap is a tool for parsing and generating Rocket League replays. It
-- is typically used as an executable; see "Rattletrap.Main" for details about
-- that interface. Using Rattletrap from Haskell is as easy as importing this
-- module. Together with "Data.ByteString.Lazy" and "Data.Binary.Get", you can
-- decode a 'Replay' value. Then you can transform it into JSON with
-- "Data.Aeson". For example:
--
-- @
-- let file = "path-to/some.replay"
-- bytes <- 'Data.ByteString.Lazy.readFile' file
-- let replay = 'Data.Binary.Get.runGet' 'Rattletrap.Replay.getReplay' bytes
-- let json = 'Data.Aeson.encode' replay
-- 'Data.ByteString.Lazy.putStr' json
-- @
--
-- There are a few unusual aspects of Rattletrap to be aware of:
--
-- - In order to improve memory usage, everything Rattletrap does is strict.
--   Don't expect any lazy values or evaluation.
-- - Some names, like 'Text', collide with commonly-used modules. Use
--   qualified imports to avoid collisions.
-- - No instances of 'Data.Binary.Binary' are provided. Use functions like
--   'getReplay' instead. This was done to improve type inference and make
--   profiling easier. Also some types require information that could not be
--   provided by the 'Data.Binary.Binary' interface.
-- - Importing one specific module is not recommended because all JSON
--   instances are defined as orphans in "Rattletrap.Json".
module Rattletrap
  ( module Rattletrap.ActorMap
  , module Rattletrap.Attribute
  , module Rattletrap.AttributeMapping
  , module Rattletrap.AttributeType
  , module Rattletrap.AttributeValue
  , module Rattletrap.Cache
  , module Rattletrap.ClassAttributeMap
  , module Rattletrap.ClassMapping
  , module Rattletrap.Content
  , module Rattletrap.Crc
  , module Rattletrap.Data
  , module Rattletrap.Frame
  , module Rattletrap.Header
  , module Rattletrap.Helper
  , module Rattletrap.Initialization
  , module Rattletrap.KeyFrame
  , module Rattletrap.Main
  , module Rattletrap.Mark
  , module Rattletrap.Message
  , module Rattletrap.Type.CompressedWord
  , module Rattletrap.Decode.CompressedWord
  , module Rattletrap.Encode.CompressedWord
  , module Rattletrap.Type.CompressedWordVector
  , module Rattletrap.Decode.CompressedWordVector
  , module Rattletrap.Encode.CompressedWordVector
  , module Rattletrap.Type.Dictionary
  , module Rattletrap.Decode.Dictionary
  , module Rattletrap.Encode.Dictionary
  , module Rattletrap.Type.Float32
  , module Rattletrap.Decode.Float32
  , module Rattletrap.Encode.Float32
  , module Rattletrap.Type.Int32
  , module Rattletrap.Decode.Int32
  , module Rattletrap.Encode.Int32
  , module Rattletrap.Type.Int8
  , module Rattletrap.Decode.Int8
  , module Rattletrap.Encode.Int8
  , module Rattletrap.Type.Int8Vector
  , module Rattletrap.Decode.Int8Vector
  , module Rattletrap.Encode.Int8Vector
  , module Rattletrap.Type.List
  , module Rattletrap.Decode.List
  , module Rattletrap.Encode.List
  , module Rattletrap.Type.Section
  , module Rattletrap.Decode.Section
  , module Rattletrap.Encode.Section
  , module Rattletrap.Type.Text
  , module Rattletrap.Decode.Text
  , module Rattletrap.Encode.Text
  , module Rattletrap.Type.Vector
  , module Rattletrap.Decode.Vector
  , module Rattletrap.Encode.Vector
  , module Rattletrap.Type.Word32
  , module Rattletrap.Decode.Word32
  , module Rattletrap.Encode.Word32
  , module Rattletrap.Type.Word64
  , module Rattletrap.Decode.Word64
  , module Rattletrap.Encode.Word64
  , module Rattletrap.Type.Word8
  , module Rattletrap.Decode.Word8
  , module Rattletrap.Encode.Word8
  , module Rattletrap.Property
  , module Rattletrap.PropertyValue
  , module Rattletrap.RemoteId
  , module Rattletrap.Replay
  , module Rattletrap.Replication
  , module Rattletrap.ReplicationValue
  , module Rattletrap.Utility
  , module Rattletrap.Version
  ) where

import Rattletrap.ActorMap
import Rattletrap.Attribute
import Rattletrap.AttributeMapping
import Rattletrap.AttributeType
import Rattletrap.AttributeValue
import Rattletrap.Cache
import Rattletrap.ClassAttributeMap
import Rattletrap.ClassMapping
import Rattletrap.Content
import Rattletrap.Crc
import Rattletrap.Data
import Rattletrap.Frame
import Rattletrap.Header
import Rattletrap.Helper
import Rattletrap.Initialization
import Rattletrap.Json ()
import Rattletrap.KeyFrame
import Rattletrap.Main
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Type.CompressedWord
import Rattletrap.Decode.CompressedWord
import Rattletrap.Encode.CompressedWord
import Rattletrap.Type.CompressedWordVector
import Rattletrap.Decode.CompressedWordVector
import Rattletrap.Encode.CompressedWordVector
import Rattletrap.Type.Dictionary
import Rattletrap.Decode.Dictionary
import Rattletrap.Encode.Dictionary
import Rattletrap.Type.Float32
import Rattletrap.Decode.Float32
import Rattletrap.Encode.Float32
import Rattletrap.Type.Int32
import Rattletrap.Decode.Int32
import Rattletrap.Encode.Int32
import Rattletrap.Type.Int8
import Rattletrap.Decode.Int8
import Rattletrap.Encode.Int8
import Rattletrap.Type.Int8Vector
import Rattletrap.Decode.Int8Vector
import Rattletrap.Encode.Int8Vector
import Rattletrap.Type.List
import Rattletrap.Decode.List
import Rattletrap.Encode.List
import Rattletrap.Type.Section
import Rattletrap.Decode.Section
import Rattletrap.Encode.Section
import Rattletrap.Type.Text
import Rattletrap.Decode.Text
import Rattletrap.Encode.Text
import Rattletrap.Type.Vector
import Rattletrap.Decode.Vector
import Rattletrap.Encode.Vector
import Rattletrap.Type.Word32
import Rattletrap.Decode.Word32
import Rattletrap.Encode.Word32
import Rattletrap.Type.Word64
import Rattletrap.Decode.Word64
import Rattletrap.Encode.Word64
import Rattletrap.Type.Word8
import Rattletrap.Decode.Word8
import Rattletrap.Encode.Word8
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.RemoteId
import Rattletrap.Replay
import Rattletrap.Replication
import Rattletrap.ReplicationValue
import Rattletrap.Utility
import Rattletrap.Version
