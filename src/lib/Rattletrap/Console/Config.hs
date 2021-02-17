module Rattletrap.Console.Config where

import qualified Rattletrap.Console.Flag as Flag
import qualified Rattletrap.Console.Mode as Mode
import qualified System.FilePath as FilePath

data Config = Config
  { compact :: Bool
  , fast :: Bool
  , help :: Bool
  , input :: Maybe String
  , mode :: Maybe Mode.Mode
  , output :: Maybe String
  , schema :: Bool
  , skipCrc :: Bool
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
  , schema = False
  , skipCrc = False
  , version = False
  }

applyFlag :: Config -> Flag.Flag -> Either String Config
applyFlag config flag = case flag of
  Flag.Compact -> Right config { compact = True }
  Flag.Fast -> Right config { fast = True }
  Flag.Help -> Right config { help = True }
  Flag.Input x -> Right config { input = Just x }
  Flag.Mode x -> do
    y <- Mode.fromString x
    Right config { mode = Just y }
  Flag.Output x -> Right config { output = Just x }
  Flag.Schema -> Right config { schema = True }
  Flag.SkipCrc -> Right config { skipCrc = True }
  Flag.Version -> Right config { version = True }

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
