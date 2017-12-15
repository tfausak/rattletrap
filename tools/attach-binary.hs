import Control.Monad (when)
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import System.FilePath (addExtension)
import System.Process (callProcess)

main :: IO ()
main = do
  token <- getEnv "GITHUB_TOKEN"
  appveyor <- lookupEnv "APPVEYOR"
  let windows = appveyor == Just "True"
  repo <- getEnv (if windows then "APPVEYOR_REPO_NAME" else "TRAVIS_REPO_SLUG")
  let
    executable = case break (== '/') repo of
      (_, '/':x) -> x
      (x, _) -> x
  tag <- getEnv (if windows then "APPVEYOR_REPO_TAG_NAME" else "TRAVIS_TAG")
  os <- if windows then pure "windows" else getEnv "TRAVIS_OS_NAME"
  callProcess "stack" ["build", "--copy-bins", "--local-bin-path", "."]
  callProcess
    "github-release"
    [ "--token"
    , token
    , "--repo"
    , repo
    , "--tag"
    , tag
    , "--file"
    , addExtension executable (if windows then "exe" else "")
    , "--name"
    , concat [executable, "-", tag, "-", os, ".gz"]
    ]
