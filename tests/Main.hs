import Control.Exception (displayException, try)
import Control.Monad (unless)
import Data.ByteString.Lazy (readFile)
import Prelude hiding (readFile)
import Rattletrap.Console.Main (rattletrap)
import System.FilePath (addExtension, joinPath)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)

import qualified Test.HUnit as Test

main :: IO Test.Counts
main = withSystemTempDirectory "rattletrap-" (Test.runTestTT . toTests)

toTests :: FilePath -> Test.Test
toTests directory = Test.TestList (fmap (toTest directory) replays)

toTest :: FilePath -> (String, String) -> Test.Test
toTest directory (uuid, name) = Test.TestLabel
  (toLabel uuid name)
  (Test.TestCase (toAssertion directory uuid))

toLabel :: String -> String -> String
toLabel = printf "%s: %s"

toAssertion :: FilePath -> String -> Test.Assertion
toAssertion directory uuid = do
  let inputFile = joinPath ["replays", addExtension uuid ".replay"]
  result <- try (readFile inputFile)
  case result of
    Left problem -> putStrLn (displayException (problem :: IOError))
    Right input -> do
      let jsonFile = joinPath [directory, addExtension uuid ".json"]
      rattletrap "" ["--compact", "--input", inputFile, "--output", jsonFile]
      let outputFile = joinPath [directory, addExtension uuid ".replay"]
      rattletrap "" ["--input", jsonFile, "--output", outputFile]
      output <- readFile outputFile
      unless
        (output == input)
        (Test.assertFailure "output does not match input")

replays :: [(String, String)]
replays =
  [ ("0008", "a flip time")
  , ("000b", "nintendo switch")
  , ("07e9", "a game mode before Neo Tokyo")
  , ("0ad2", "some Latin-1 text")
  , ("1205", "rumble mode")
  , ("160c", "a dedicated server IP")
  , ("16d5", "new property types")
  , ("18d6", "an online loadout attribute")
  , ("1a12", "overtime")
  , ("1ae4", "a game time")
  , ("1bc2", "no padding after the frames")
  , ("1d1d", "a camera pitch")
  , ("1ef9", "a private hoops match")
  , ("1f37", "splitscreen players")
  , ("2114", "a match save")
  , ("2266", "dropshot")
  , ("22ba", "a vote to forfeit")
  , ("27b6", "some UTF-16 text")
  , ("29f5", "frames")
  , ("2cfe", "a new playstation id")
  , ("3381", "patch 1.37")
  , ("372d", "a camera yaw attribute")
  , ("387f", "a frozen attribute")
  , ("3ea1", "a custom team name")
  , ("4126", "a game mode after Neo Tokyo")
  , ("42f0", "reservations after Neo Tokyo")
  , ("504e", "some messages")
  , ("520e", "no pickup attribute")
  , ("52aa", "a match-ending attribute")
  , ("540d", "a demolish attribute")
  , ("551c", "private match settings")
  , ("6210", "different player history key")
  , ("6320", "a forfeit attribute")
  , ("6688", "a malformed byte property")
  , ("6b0d", "patch 1.37")
  , ("6d1b", "a flip right")
  , ("6f7c", "a map with numbers")
  , ("7083", "weird basketball capitalization")
  , ("7109", "a boost modifier")
  , ("7bf6", "an online loadouts attribute")
  , ("89cb", "remote user data")
  , ("8ae5", "new painted items")
  , ("9704", "a batarang")
  , ("98e5", "a player using behind view")
  , ("a128", "a round count down")
  , ("a52f", "some more mutators")
  , ("a558", "extended explosion data")
  , ("a671", "a waiting player")
  , ("a7f0", "a ready attribute")
  , ("afb1", "patch 1.37")
  , ("b9f9", "a party leader")
  , ("c14f", "some mutators")
  , ("c837", "a spectator")
  , ("cc4c", "after Starbase ARC")
  , ("d044", "hoops mutators")
  , ("d428", "a private hockey match")
  , ("d7fb", "an explosion attribute")
  , ("dcb3", "a pawn type attribute")
  , ("de56", "a problematic product attribute")
  , ("eae3", "an actor/object ID collision")
  , ("eae8", "custom team colors")
  , ("f299", "a location attribute")
  , ("f7b9", "a hockey game event")
  , ("f811", "no frames")
  , ("fdc7", "an MVP")
  ]
