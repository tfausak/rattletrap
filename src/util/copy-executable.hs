module Main
  ( main
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  original <- Process.readProcess "which" ["rattletrap"] ""
  let
    source = convert $ List.dropWhileEnd Char.isSpace original
    directory = "docker"
    target = FilePath.combine directory "rattletrap"
  putStrLn $ mconcat
    [ "Copying "
    , show source
    , " ("
    , show original
    , ") to "
    , show target
    , " ..."
    ]
  Directory.createDirectoryIfMissing True directory
  Directory.copyFile source target

convert :: FilePath -> FilePath
convert path = case path of
  '/' : drive : '/' : rest -> Char.toUpper drive : ':' : '/' : rest <> ".exe"
  _ -> path
