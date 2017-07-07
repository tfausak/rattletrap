{- stack --resolver lts-8.0 script
  --package aeson
  --package bytestring
  --package directory
  --package filepath
  --package process
  --package text
-}
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  checkTravisTag
  checkTravisOsName
  directory <- getStackUploadDirectory
  credentials <- getHackageCredentials
  let file = FilePath.joinPath [directory, "credentials.json"]
  Directory.createDirectoryIfMissing True directory
  ByteString.writeFile file (Aeson.encode credentials)
  Process.callProcess "stack" ["upload", "."]

checkTravisTag :: IO ()
checkTravisTag = do
  maybeTag <- Environment.lookupEnv "TRAVIS_TAG"
  case maybeTag of
    Just (_:_) -> pure ()
    _ -> do
      putStrLn "The $TRAVIS_TAG variable is empty or not set."
      Exit.exitSuccess

checkTravisOsName :: IO ()
checkTravisOsName = do
  maybeOs <- Environment.lookupEnv "TRAVIS_OS_NAME"
  Monad.when
    (maybeOs /= Just "linux")
    (do putStrLn "The $TRAVIS_OS_NAME variable is not 'linux'."
        Exit.exitSuccess)

getStackUploadDirectory :: IO FilePath
getStackUploadDirectory = do
  home <- Directory.getHomeDirectory
  pure (FilePath.joinPath [home, ".stack", "upload"])

getHackageCredentials :: IO Aeson.Value
getHackageCredentials = do
  username <- Environment.getEnv "HACKAGE_USERNAME"
  password <- Environment.getEnv "HACKAGE_PASSWORD"
  pure
    (Aeson.object
       [ Text.pack "username" Aeson..= username
       , Text.pack "password" Aeson..= password
       ])
