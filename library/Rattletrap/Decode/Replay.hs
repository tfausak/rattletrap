module Rattletrap.Decode.Replay
  ( decodeReplay
  ) where

import Rattletrap.Decode.Common
import Rattletrap.Decode.Content
import Rattletrap.Decode.Header
import Rattletrap.Decode.Section
import Rattletrap.Type.Header
import Rattletrap.Type.Replay
import Rattletrap.Type.Section

decodeReplay :: Decode Replay
decodeReplay = do
  header <- decodeSection decodeHeader
  Replay header <$> decodeSection
    ( decodeContent
      (getVersion (sectionBody header))
      (getNumFrames (sectionBody header))
      (getMaxChannels (sectionBody header))
    )
