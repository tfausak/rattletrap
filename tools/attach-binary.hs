{- stack runghc
  --package bytestring
  --package github-release
  --package process
  --package temporary
  --package zlib
  -- -Wall
-}
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char
import qualified GitHubRelease
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

main :: IO ()
main = do
  token <- getToken
  environment <- getEnvironment
  (owner, repo) <- getOwnerAndRepo environment
  tag <- getTag environment
  let executable = repo
  file <- getFile executable
  name <- getName executable tag environment
  upload token owner repo tag file name

getToken :: IO String
getToken = Environment.getEnv "GITHUB_TOKEN"

getEnvironment :: IO Environment
getEnvironment = do
  appVeyor <- Environment.lookupEnv "APPVEYOR"
  travisCI <- Environment.lookupEnv "TRAVIS"
  case (appVeyor, travisCI) of
    (Just "True", _) -> pure AppVeyor
    (_, Just "true") -> pure TravisCI
    _ -> Exit.die "Neither APPVEYOR=True nor TRAVIS=true are set."

data Environment
  = AppVeyor
  | TravisCI
  deriving (Eq, Show)

getOwnerAndRepo :: Environment -> IO (String, String)
getOwnerAndRepo environment = do
  let name =
        case environment of
          AppVeyor -> "APPVEYOR_REPO_NAME"
          TravisCI -> "TRAVIS_REPO_SLUG"
  slug <- Environment.getEnv name
  let (owner, rawRepo) = break (== '/') slug
  let repo = drop 1 rawRepo
  pure (owner, repo)

getTag :: Environment -> IO String
getTag environment = do
  let name =
        case environment of
          AppVeyor -> "APPVEYOR_REPO_TAG_NAME"
          TravisCI -> "TRAVIS_TAG"
  Environment.getEnv name

getFile :: String -> IO String
getFile executable = do
  output <- Process.readProcess "stack" ["exec", "which", executable] ""
  let chomp = reverse . dropWhile Char.isSpace . reverse
  pure (chomp output)

getName :: String -> String -> Environment -> IO String
getName executable tag environment = do
  os <-
    case environment of
      AppVeyor -> pure "windows"
      TravisCI -> Environment.getEnv "TRAVIS_OS_NAME"
  pure (concat [executable, "-", tag, "-", os, ".gz"])

upload :: String -> String -> String -> String -> FilePath -> String -> IO ()
upload token owner repo tag file name = do
  input <- ByteString.readFile file
  let output = GZip.compress input
  Temp.withSystemTempFile
    "archive.gz"
    (\archive handle -> do
       IO.hClose handle
       ByteString.writeFile archive output
       GitHubRelease.upload token owner repo tag file name)
