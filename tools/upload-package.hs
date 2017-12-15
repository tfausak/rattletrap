import Control.Monad (when)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (writeFile)
import Data.Text (pack)
import Prelude hiding (writeFile)
import System.Environment (getEnv)
import System.Exit (die)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath (joinPath)
import System.Process (callProcess)

main :: IO ()
main = do
  tag <- getEnv "TRAVIS_TAG"
  os <- getEnv "TRAVIS_OS_NAME"
  user <- getEnv "HACKAGE_USERNAME"
  pass <- getEnv "HACKAGE_PASSWORD"
  when (null tag) (die "empty $TRAVIS_TAG")
  when (os /= "linux") (die "wrong $TRAVIS_OS_NAME")
  when (null user) (die "empty $HACKAGE_USERNAME")
  when (null pass) (die "empty $HACKAGE_PASSWORD")
  home <- getHomeDirectory
  let directory = joinPath [home, ".stack", "upload"]
  createDirectoryIfMissing True directory
  writeFile
    (joinPath [directory, "credentials.json"])
    (encode (object [pack "username" .= user, pack "password" .= pass]))
