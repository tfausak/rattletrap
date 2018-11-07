import Data.Aeson ((.=), encode, object)
import Data.ByteString.Lazy (writeFile)
import Data.Text (pack)
import Prelude hiding (writeFile)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath (joinPath)
import System.Process (callProcess)

main :: IO ()
main = do
  _ <- getEnv "TRAVIS_TAG"
  "linux" <- getEnv "TRAVIS_OS_NAME"
  user <- getEnv "HACKAGE_USERNAME"
  pass <- getEnv "HACKAGE_PASSWORD"
  home <- getHomeDirectory
  let directory = joinPath [home, ".stack", "upload"]
  createDirectoryIfMissing True directory
  writeFile
    (joinPath [directory, "credentials.json"])
    (encode (object [pack "username" .= user, pack "password" .= pass]))
  callProcess "stack" ["upload", "--no-signature", "."]
