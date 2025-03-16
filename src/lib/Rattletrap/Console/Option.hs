module Rattletrap.Console.Option where

import qualified Rattletrap.Console.Flag as Flag
import qualified System.Console.GetOpt as Console

type Option = Console.OptDescr Flag.Flag

all :: [Option]
all = [compact, fast, help, input, mode, output, schema, skipCrc, version]

compact :: Option
compact =
  Console.Option
    ['c']
    ["compact"]
    (Console.NoArg Flag.Compact)
    "minify JSON output"

fast :: Option
fast =
  Console.Option
    ['f']
    ["fast"]
    (Console.NoArg Flag.Fast)
    "skip encoding or decoding the frames"

help :: Option
help = Console.Option ['h'] ["help"] (Console.NoArg Flag.Help) "show the help"

input :: Option
input =
  Console.Option
    ['i']
    ["input"]
    (Console.ReqArg Flag.Input "FILE|URL")
    "input file or URL"

mode :: Option
mode =
  Console.Option
    ['m']
    ["mode"]
    (Console.ReqArg Flag.Mode "MODE")
    "decode or encode"

output :: Option
output =
  Console.Option
    ['o']
    ["output"]
    (Console.ReqArg Flag.Output "FILE")
    "output file"

schema :: Option
schema =
  Console.Option [] ["schema"] (Console.NoArg Flag.Schema) "output the schema"

skipCrc :: Option
skipCrc =
  Console.Option [] ["skip-crc"] (Console.NoArg Flag.SkipCrc) "skip the CRC"

version :: Option
version =
  Console.Option
    ['v']
    ["version"]
    (Console.NoArg Flag.Version)
    "show the version"
