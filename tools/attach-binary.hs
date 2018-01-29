import System.Environment (getEnv, lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath (addExtension)
import System.Process (callProcess)

main :: IO ()
main = do
  appveyor <- lookupEnv "APPVEYOR"
  let windows = appveyor == Just "True"
  maybeTag <- lookupEnv
    (if windows then "APPVEYOR_REPO_TAG_NAME" else "TRAVIS_TAG")
  tag <- maybe exitSuccess pure maybeTag
  token <- getEnv "GITHUB_TOKEN"
  repo <- getEnv (if windows then "APPVEYOR_REPO_NAME" else "TRAVIS_REPO_SLUG")
  let
    executable = case break (== '/') repo of
      (_, '/':x) -> x
      (x, _) -> x
  os <- if windows then pure "windows" else getEnv "TRAVIS_OS_NAME"
  callProcess "stack" ["build", "--copy-bins", "--local-bin-path", "."]
  callProcess
    "github-release"
    [ "upload"
    , "--token"
    , token
    , "--repo"
    , repo
    , "--tag"
    , tag
    , "--file"
    , addExtension executable (if windows then "exe" else "")
    , "--name"
    , addExtension (concat [executable, "-", tag, "-", os]) (if windows then "exe" else "")
    ]
