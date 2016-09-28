module Main
  ( main
  ) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Rattletrap
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hspec as Hspec

main :: IO ()
main = do
  tests <- Hspec.testSpec "rattletrap" (Hspec.parallel spec)
  Tasty.defaultMain tests

spec :: Hspec.Spec
spec = Hspec.describe "Rattletrap" (mapM_ (uncurry itCanRoundTrip) replays)

itCanRoundTrip :: String -> String -> Hspec.Spec
itCanRoundTrip uuid description =
  Hspec.it
    (labelFor description)
    (do let file = pathToReplay uuid
        (input, _replay, output) <- process file
        Hspec.shouldBe output input)

labelFor :: String -> String
labelFor description = "can round trip " ++ description

pathToReplay :: String -> FilePath
pathToReplay uuid =
  FilePath.joinPath ["test", "replays", FilePath.addExtension uuid ".replay"]

process
  :: FilePath
  -> IO (LazyByteString.ByteString, Rattletrap.Replay, LazyByteString.ByteString)
process file = do
  input <- LazyByteString.readFile file
  let replay = Binary.runGet Rattletrap.getReplay input
  let output = Binary.runPut (Rattletrap.putReplay replay)
  pure (input, replay, output)

replays :: [(String, String)]
replays =
  [ ("F811C1D24888015E23B598AD8628C742", "a replay without frames")
  , ("29F582C34A65EB34D358A784CBE3C189", "a replay with frames")
  ]
