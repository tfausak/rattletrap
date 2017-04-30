module Rattletrap.Attribute.Titles where

import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO.Unsafe
import Text.Read

import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit

data TitlesAttribute = TitlesAttribute
  {
  } deriving (Eq, Ord, Show)

getTitlesAttribute :: BinaryBit.BitGet TitlesAttribute
getTitlesAttribute = do
  {-
    TODO: This is grasping at straws.

    Promising values in the range [0, 128] for NUM_BITS:

    - 33:  TAGame.PRI_TA:ReplacingBotPRI
    - 45:  TAGame.PRI_TA:ClientChangeTeamFailed
    - 54:  TAGame.PRI_TA:ClientChangeTeamFailed
    - 66:  TAGame.PRI_TA:ServerChangeTeam
    - 107: TAGame.PRI_TA:ServerReadyUp
    - 111: TAGame.PRI_TA:ClientNotifyGainedStat
    - 116: TAGame.PRI_TA:ServerSplitScreenStatusChanged
    - 119: TAGame.PRI_TA:ClientNotifyGainedStat
    - 123: TAGame.PRI_TA:ClientGenerateItemDrop
  -}
  let
    a = lookupEnv "NUM_BITS"
    b = unsafePerformIO a
    c = fromMaybe "" b
    d = readMaybe c
    numBits = fromMaybe 0 d
  traceShowM ("NUM_BITS", numBits)
  mapM_ (const BinaryBit.getBool) (replicate numBits ())
  pure TitlesAttribute

putTitlesAttribute :: TitlesAttribute -> BinaryBit.BitPut ()
putTitlesAttribute _ = pure ()
