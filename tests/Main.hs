module Main
  ( main
  )
where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.Int as Int
import qualified Rattletrap.Console.Main as Rattletrap
import qualified System.Clock as Clock
import qualified System.Exit as Exit
import qualified System.FilePath as Path
import qualified System.IO.Temp as Temp
import qualified System.Mem as Mem
import qualified Test.HUnit as Test
import qualified Text.Printf as Printf

main :: IO ()
main = Temp.withSystemTempDirectory "rattletrap-" (runTests . toTests)

runTests :: Test.Test -> IO ()
runTests test = do
  result <- Test.runTestTT test
  Monad.when
    (Test.errors result > 0 || Test.failures result > 0)
    Exit.exitFailure

toTests :: FilePath -> Test.Test
toTests directory = Test.TestList (fmap (toTest directory) replays)

toTest :: FilePath -> (String, String) -> Test.Test
toTest directory (uuid, name) = Test.TestLabel
  (toLabel uuid name)
  (Test.TestCase (toAssertion directory uuid))

toLabel :: String -> String -> String
toLabel uuid name = uuid <> ": " <> name

toAssertion :: FilePath -> String -> Test.Assertion
toAssertion directory uuid = do
  let
    inputFile = Path.joinPath ["replays", Path.addExtension uuid ".replay"]
    jsonFile = Path.joinPath [directory, Path.addExtension uuid ".json"]
    outputFile = Path.joinPath [directory, Path.addExtension uuid ".replay"]
  input <- Bytes.readFile inputFile
  putStrLn ("\t" <> uuid)
  do
    (((), allocated), elapsed) <- withElapsed
      (withAllocations (decode inputFile jsonFile))
    put "decoding" (Bytes.length input) (Clock.toNanoSecs elapsed) allocated
  do
    (((), allocated), elapsed) <- withElapsed
      (withAllocations (encode jsonFile outputFile))
    put "encoding" (Bytes.length input) (Clock.toNanoSecs elapsed) allocated
  output <- Bytes.readFile outputFile
  Monad.unless
    (output == input)
    (Test.assertFailure "output does not match input")

put :: String -> Int.Int64 -> Integer -> Int.Int64 -> IO ()
put label size elapsed allocated = Printf.printf
  "%s %d byte%s took %d nanosecond%s (%.3f MB/s) and allocated %d byte%s (%d x)\n"
  label
  size
  (if size == 1 then "" else "s")
  elapsed
  (if elapsed == 1 then "" else "s")
  ((1e9 * fromIntegral size) / (1048576 * fromIntegral elapsed) :: Double)
  allocated
  (if allocated == 1 then "" else "s")
  (div allocated size)

decode :: FilePath -> FilePath -> IO ()
decode input output =
  Rattletrap.rattletrap "" ["--compact", "--input", input, "--output", output]

encode :: FilePath -> FilePath -> IO ()
encode input output =
  Rattletrap.rattletrap "" ["--input", input, "--output", output]

withAllocations :: IO a -> IO (a, Int.Int64)
withAllocations action = do
  before <- Mem.getAllocationCounter
  result <- action
  after <- Mem.getAllocationCounter
  pure (result, before - after)

withElapsed :: IO a -> IO (a, Clock.TimeSpec)
withElapsed action = do
  before <- Clock.getTime Clock.Monotonic
  result <- action
  after <- Clock.getTime Clock.Monotonic
  pure (result, Clock.diffTimeSpec before after)

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
  , ("3abd", "rlcs")
  , ("3ea1", "a custom team name")
  , ("4126", "a game mode after Neo Tokyo")
  , ("419a", "a club match")
  , ("42f0", "reservations after Neo Tokyo")
  , ("4bc3", "with timed out attribute")
  , ("504e", "some messages")
  , ("520e", "no pickup attribute")
  , ("524f", "quat edge case")
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
  , ("7256", "special edition")
  , ("75ce", "primary and secondary titles")
  , ("7bf6", "an online loadouts attribute")
  , ("89cb", "remote user data")
  , ("8ae5", "new painted items")
  , ("92a6", "with server performance state")
  , ("946f", "patch 1.43")
  , ("9704", "a batarang")
  , ("98e5", "a player using behind view")
  , ("a09e", "a tournament")
  , ("a128", "a round count down")
  , ("a52f", "some more mutators")
  , ("a558", "extended explosion data")
  , ("a671", "a waiting player")
  , ("a676", "new user color")
  , ("a7f0", "a ready attribute")
  , ("a9df", "salty shores patch 1.45")
  , ("aa70", "patch 1.50 - TitleID attribute")
  , ("afb1", "patch 1.37")
  , ("b9f9", "a party leader")
  , ("c14f", "some mutators")
  , ("c837", "a spectator")
  , ("cc4c", "after Starbase ARC")
  , ("d044", "hoops mutators")
  , ("d236", "rlcs s2")
  , ("d428", "a private hockey match")
  , ("d52e", "psynet system id")
  , ("d7fb", "an explosion attribute")
  , ("db70", "new lag indicator")
  , ("dcb3", "a pawn type attribute")
  , ("de56", "a problematic product attribute")
  , ("e80d", "unlimited time")
  , ("eae3", "an actor/object ID collision")
  , ("eae8", "custom team colors")
  , ("f299", "a location attribute")
  , ("f7b9", "a hockey game event")
  , ("f811", "no frames")
  , ("fdc7", "an MVP")
  ]
