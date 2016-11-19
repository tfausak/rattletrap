-- | Rattletrap is a tool for parsing and generating Rocket League replays. It
-- is typically used as an executable; see "Rattletrap.Main" for details about
-- that interface. Using Rattletrap from Haskell is as easy as importing this
-- module. Together with "Data.ByteString.Lazy" and "Data.Binary", you can
-- decode a 'Replay' value. Then you can transform it into JSON with
-- "Data.Aeson". For example:
--
-- > import qualified Data.Aeson as Aeson
-- > import qualified Data.Binary as Binary
-- > import qualified Data.ByteString.Lazy as ByteString
-- > import qualified Rattletrap
-- >
-- > main = do
-- >   let file = "path-to/some.replay"
-- >   bytes <- ByteString.readFile file
-- >   let replay = Binary.runGet Rattletrap.getReplay bytes
-- >   let json = Aeson.encode replay
-- >   ByteString.putStr json
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
  , module Rattletrap.Initialization
  , module Rattletrap.KeyFrame
  , module Rattletrap.Main
  , module Rattletrap.Mark
  , module Rattletrap.Message
  , module Rattletrap.Primitive
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
import Rattletrap.Initialization
import Rattletrap.Json ()
import Rattletrap.KeyFrame
import Rattletrap.Main
import Rattletrap.Mark
import Rattletrap.Message
import Rattletrap.Primitive
import Rattletrap.Property
import Rattletrap.PropertyValue
import Rattletrap.RemoteId
import Rattletrap.Replay
import Rattletrap.Replication
import Rattletrap.ReplicationValue
import Rattletrap.Utility
import Rattletrap.Version
