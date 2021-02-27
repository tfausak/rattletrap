module Rattletrap.Version where

import qualified Data.Version as Version
import qualified Paths_rattletrap as Package

string :: String
string = Version.showVersion Package.version
