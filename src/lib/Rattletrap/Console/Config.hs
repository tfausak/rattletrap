module Rattletrap.Console.Config where

import qualified Rattletrap.Console.Mode as Mode
import qualified System.FilePath as FilePath

data Config = Config
  { compact :: Bool
  , fast :: Bool
  , help :: Bool
  , input :: Maybe String
  , mode :: Maybe Mode.Mode
  , output :: Maybe String
  , version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial = Config
  { compact = False
  , fast = False
  , help = False
  , input = Nothing
  , mode = Nothing
  , output = Nothing
  , version = False
  }

getMode :: Config -> Mode.Mode
getMode config =
  let
    i = FilePath.takeExtension <$> input config
    o = FilePath.takeExtension <$> output config
  in case (i, o) of
    (Just ".json", _) -> Mode.Encode
    (Just ".replay", _) -> Mode.Decode
    (_, Just ".json") -> Mode.Decode
    (_, Just ".replay") -> Mode.Encode
    _ -> Mode.Decode
