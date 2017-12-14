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
  ( module Rattletrap.Type.ActorMap
  , module Rattletrap.Type.Attribute
  , module Rattletrap.Decode.Attribute
  , module Rattletrap.Encode.Attribute
  , module Rattletrap.Type.AttributeMapping
  , module Rattletrap.Decode.AttributeMapping
  , module Rattletrap.Encode.AttributeMapping
  , module Rattletrap.Type.AttributeType
  , module Rattletrap.Type.AttributeValue
  , module Rattletrap.Decode.AttributeValue
  , module Rattletrap.Encode.AttributeValue
  , module Rattletrap.Type.Cache
  , module Rattletrap.Decode.Cache
  , module Rattletrap.Encode.Cache
  , module Rattletrap.Type.ClassAttributeMap
  , module Rattletrap.Type.ClassMapping
  , module Rattletrap.Decode.ClassMapping
  , module Rattletrap.Encode.ClassMapping
  , module Rattletrap.Type.Content
  , module Rattletrap.Decode.Content
  , module Rattletrap.Encode.Content
  , module Rattletrap.Crc
  , module Rattletrap.Data
  , module Rattletrap.Type.Frame
  , module Rattletrap.Decode.Frame
  , module Rattletrap.Encode.Frame
  , module Rattletrap.Type.Header
  , module Rattletrap.Decode.Header
  , module Rattletrap.Encode.Header
  , module Rattletrap.Helper
  , module Rattletrap.Type.Initialization
  , module Rattletrap.Decode.Initialization
  , module Rattletrap.Encode.Initialization
  , module Rattletrap.Type.KeyFrame
  , module Rattletrap.Decode.KeyFrame
  , module Rattletrap.Encode.KeyFrame
  , module Rattletrap.Main
  , module Rattletrap.Type.Mark
  , module Rattletrap.Decode.Mark
  , module Rattletrap.Encode.Mark
  , module Rattletrap.Type.Message
  , module Rattletrap.Decode.Message
  , module Rattletrap.Encode.Message
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
  , module Rattletrap.Type.Property
  , module Rattletrap.Decode.Property
  , module Rattletrap.Encode.Property
  , module Rattletrap.Type.PropertyValue
  , module Rattletrap.Decode.PropertyValue
  , module Rattletrap.Encode.PropertyValue
  , module Rattletrap.Type.RemoteId
  , module Rattletrap.Decode.RemoteId
  , module Rattletrap.Encode.RemoteId
  , module Rattletrap.Type.Replay
  , module Rattletrap.Decode.Replay
  , module Rattletrap.Encode.Replay
  , module Rattletrap.Type.Replication
  , module Rattletrap.Decode.Replication
  , module Rattletrap.Encode.Replication
  , module Rattletrap.Type.ReplicationValue
  , module Rattletrap.Decode.ReplicationValue
  , module Rattletrap.Encode.ReplicationValue
  , module Rattletrap.Utility
  , module Rattletrap.Version
  ) where

import Rattletrap.Type.ActorMap
import Rattletrap.Type.Attribute
import Rattletrap.Decode.Attribute
import Rattletrap.Encode.Attribute
import Rattletrap.Type.AttributeMapping
import Rattletrap.Decode.AttributeMapping
import Rattletrap.Encode.AttributeMapping
import Rattletrap.Type.AttributeType
import Rattletrap.Type.AttributeValue
import Rattletrap.Decode.AttributeValue
import Rattletrap.Encode.AttributeValue
import Rattletrap.Type.Cache
import Rattletrap.Decode.Cache
import Rattletrap.Encode.Cache
import Rattletrap.Type.ClassAttributeMap
import Rattletrap.Type.ClassMapping
import Rattletrap.Decode.ClassMapping
import Rattletrap.Encode.ClassMapping
import Rattletrap.Type.Content
import Rattletrap.Decode.Content
import Rattletrap.Encode.Content
import Rattletrap.Crc
import Rattletrap.Data
import Rattletrap.Type.Frame
import Rattletrap.Decode.Frame
import Rattletrap.Encode.Frame
import Rattletrap.Type.Header
import Rattletrap.Decode.Header
import Rattletrap.Encode.Header
import Rattletrap.Helper
import Rattletrap.Type.Initialization
import Rattletrap.Decode.Initialization
import Rattletrap.Encode.Initialization
import Rattletrap.Json ()
import Rattletrap.Type.KeyFrame
import Rattletrap.Decode.KeyFrame
import Rattletrap.Encode.KeyFrame
import Rattletrap.Main
import Rattletrap.Type.Mark
import Rattletrap.Decode.Mark
import Rattletrap.Encode.Mark
import Rattletrap.Type.Message
import Rattletrap.Decode.Message
import Rattletrap.Encode.Message
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
import Rattletrap.Type.Property
import Rattletrap.Decode.Property
import Rattletrap.Encode.Property
import Rattletrap.Type.PropertyValue
import Rattletrap.Decode.PropertyValue
import Rattletrap.Encode.PropertyValue
import Rattletrap.Type.RemoteId
import Rattletrap.Decode.RemoteId
import Rattletrap.Encode.RemoteId
import Rattletrap.Type.Replay
import Rattletrap.Decode.Replay
import Rattletrap.Encode.Replay
import Rattletrap.Type.Replication
import Rattletrap.Decode.Replication
import Rattletrap.Encode.Replication
import Rattletrap.Type.ReplicationValue
import Rattletrap.Decode.ReplicationValue
import Rattletrap.Encode.ReplicationValue
import Rattletrap.Utility
import Rattletrap.Version
