module Main ( main ) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Process as Process

main :: IO ()
main = do
  putStrLn "Copying executable ..."
  before <- Process.readProcess "which" [ "rattletrap" ] ""
  print before
  let after = convert $ List.dropWhileEnd Char.isSpace before
  print after
  Directory.copyFile after "output/rattletrap"

convert :: FilePath -> FilePath
convert path = case path of
  '/' : drive : '/' : rest ->
    Char.toUpper drive
    : ':'
    : '\\'
    : fmap (\ x -> if x == '/' then '\\' else x) rest
    <> ".exe"
  _ -> path
