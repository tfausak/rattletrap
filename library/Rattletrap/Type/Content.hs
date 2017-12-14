{-# LANGUAGE TemplateHaskell #-}

module Rattletrap.Type.Content
  ( Content(..)
  ) where

import Rattletrap.Type.Common
import Rattletrap.Type.Cache
import Rattletrap.Type.ClassMapping
import Rattletrap.Type.Frame
import Rattletrap.Type.KeyFrame
import Rattletrap.Type.Mark
import Rattletrap.Type.Message
import Rattletrap.Type.List
import Rattletrap.Type.Text
import Rattletrap.Type.Word32

-- | Contains low-level game data about a 'Rattletrap.Replay.Replay'.
data Content = Content
  { contentLevels :: List Text
  -- ^ This typically only has one element, like @stadium_oob_audio_map@.
  , contentKeyFrames :: List KeyFrame
  -- ^ A list of which frames are key frames. Although they aren't necessary
  -- for replay, key frames are frames that replicate every actor. They
  -- typically happen once every 10 seconds.
  , contentStreamSize :: Word32
  -- ^ The size of the stream in bytes. This is only really necessary because
  -- the stream has some arbitrary amount of padding at the end.
  , contentFrames :: [Frame]
  -- ^ The actual game data. This is where all the interesting information is.
  , contentMessages :: List Message
  -- ^ Debugging messages. In newer replays, this is always empty.
  , contentMarks :: List Mark
  -- ^ Tick marks shown on the scrubber when watching a replay.
  , contentPackages :: List Text
  -- ^ A list of @.upk@ files to load, like
  -- @..\\..\\TAGame\\CookedPCConsole\\Stadium_P.upk@.
  , contentObjects :: List Text
  -- ^ Objects in the stream. Used for the
  -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , contentNames :: List Text
  -- ^ It's not clear what these are used for. This list is usually not empty,
  -- but appears unused otherwise.
  , contentClassMappings :: List ClassMapping
  -- ^ A mapping between classes and their ID in the stream. Used for the
  -- 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  , contentCaches :: List Cache
  -- ^ A list of classes along with their parent classes and attributes. Used
  -- for the 'Rattletrap.Type.ClassAttributeMap.ClassAttributeMap'.
  } deriving (Eq, Ord, Show)

$(deriveJson ''Content)
