module Main
  ( main
  ) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Rattletrap
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hspec as Hspec

main :: IO ()
main = do
  tests <- Hspec.testSpec "rattletrap" (Hspec.parallel spec)
  Tasty.defaultMain tests

spec :: Hspec.Spec
spec =
  Hspec.describe
    "Rattletrap"
    (Hspec.context "get and put" (mapM_ (uncurry itCanGetAndPut) replays))

itCanGetAndPut :: String -> String -> Hspec.Spec
itCanGetAndPut uuid description =
  Hspec.it
    ("a replay " ++ description)
    (do let file = pathToReplay uuid
        (input, _, output) <- getAndPut file
        Hspec.shouldBe output input)

pathToReplay :: String -> FilePath
pathToReplay uuid =
  FilePath.joinPath ["test", "replays", FilePath.addExtension uuid ".replay"]

getAndPut :: FilePath
          -> IO (ByteString.ByteString, Rattletrap.Replay, ByteString.ByteString)
getAndPut file = do
  input <- ByteString.readFile file
  let replay = Binary.runGet Rattletrap.getReplay input
  let output = Binary.runPut (Rattletrap.putReplay replay)
  pure (input, replay, output)

replays :: [(String, String)]
replays =
  [ ("F811C1D24888015E23B598AD8628C742", "with no frames")
  , ("29F582C34A65EB34D358A784CBE3C189", "with frames")
  , ("6688EEE34BFEB3EC3A9E3283098CC712", "with a weird byte property")
  , ("18D6738D415B70B5BE4C299588D3C141", "with an online loadout")
  ]
