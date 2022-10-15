import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified GHC.Clock as Clock
import qualified Rattletrap
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf

main :: IO ()
main = do
  let directory = "output"
  generateSchema directory
  mapM_ (testReplay directory) replays

generateSchema :: FilePath -> IO ()
generateSchema directory = Rattletrap.rattletrap
  ""
  ["--schema", "--output", FilePath.combine directory "schema.json"]

testReplay :: FilePath -> (FilePath, String) -> IO ()
testReplay directory (uuid, name) = do
  let inputFile = FilePath.combine "replays" $ uuid <> ".replay"
  input <- ByteString.readFile inputFile
  let mb = fromIntegral (ByteString.length input) / (1024 * 1024 :: Double)
  Printf.printf "- replay: %s %s (%.3f mb)\n" uuid name mb
  let jsonFile = FilePath.combine directory $ uuid <> ".json"
  do
    (s, ()) <- withDuration $ decode inputFile jsonFile
    Printf.printf "  decode: %.3f s @ %.3f mb/s\n" s (mb / s)
  let outputFile = FilePath.combine directory $ uuid <> ".replay"
  do
    (s, ()) <- withDuration $ encode jsonFile outputFile
    Printf.printf "  encode: %.3f s @ %.3f mb/s\n" s (mb / s)
  output <- ByteString.readFile outputFile
  Monad.when (output /= input) $ fail "output does not match input"

withDuration :: IO a -> IO (Double, a)
withDuration action = do
  before <- Clock.getMonotonicTime
  result <- action
  after <- Clock.getMonotonicTime
  pure (after - before, result)

decode :: FilePath -> FilePath -> IO ()
decode input output =
  Rattletrap.rattletrap "" ["--compact", "--input", input, "--output", output]

encode :: FilePath -> FilePath -> IO ()
encode input output =
  Rattletrap.rattletrap "" ["--input", input, "--output", output]

replays :: [(String, String)]
replays =
  [ ("0008", "a flip time") -- https://github.com/tfausak/rattletrap/commit/ee7afa0
  , ("000b", "nintendo switch") -- https://github.com/tfausak/rattletrap/pull/60
  , ("0121", "RLCS 2") -- https://github.com/nickbabcock/boxcars/pull/120
  , ("0416", "v1.78 demolition") -- https://github.com/tfausak/rattletrap/pull/164
  , ("07e9", "a game mode before Neo Tokyo") -- https://github.com/tfausak/rattletrap/commit/b806f9b
  , ("0ad2", "some Latin-1 text") -- https://github.com/tfausak/rattletrap/commit/13a8b2d
  , ("0ca5", "with QQ remote ID") -- https://github.com/nickbabcock/boxcars/pull/69
  , ("0e76", "v1.95 rumble") -- https://github.com/tfausak/rattletrap/pull/237
  , ("1205", "rumble mode") -- https://github.com/tfausak/rattletrap/commit/5256500
  , ("160c", "a dedicated server IP") -- https://github.com/tfausak/rattletrap/commit/5c64a6d
  , ("16d5", "new property types") -- https://github.com/tfausak/rattletrap/pull/41
  , ("18d6", "an online loadout attribute") -- https://github.com/tfausak/rattletrap/commit/a9900e7
  , ("1a12", "overtime") -- https://github.com/tfausak/rattletrap/commit/ada6053
  , ("1ae4", "a game time") -- https://github.com/tfausak/rattletrap/commit/d08176e
  , ("1bc2", "no padding after the frames") -- https://github.com/tfausak/rattletrap/commit/c9a2dd8
  , ("1d1d", "a camera pitch") -- https://github.com/tfausak/rattletrap/commit/ee7afa0
  , ("1ec9", "a V1.63 match") -- https://github.com/tfausak/rattletrap/pull/132
  , ("1ef9", "a private hoops match") -- https://github.com/tfausak/rattletrap/commit/5570839
  , ("1f37", "splitscreen players") -- https://github.com/tfausak/rattletrap/commit/c4d2f32
  , ("2114", "a match save") -- https://github.com/tfausak/rattletrap/commit/ee7afa0
  , ("21a8", "v1.66") -- https://github.com/tfausak/rattletrap/pull/142
  , ("2266", "dropshot") -- https://github.com/tfausak/rattletrap/pull/36
  , ("22ba", "a vote to forfeit") -- https://github.com/tfausak/rattletrap/commit/86656bb
  , ("27b6", "some UTF-16 text") -- https://github.com/tfausak/rattletrap/commit/c4d2f32
  , ("29f5", "frames") -- https://github.com/tfausak/rattletrap/commit/bf4b6af
  , ("2cf8", "demo v2.01") -- https://github.com/tfausak/rattletrap/pull/243
  , ("2cfe", "a new playstation id") -- https://github.com/tfausak/rattletrap/issues/51
  , ("3381", "patch 1.37") -- https://github.com/tfausak/rattletrap/pull/48
  , ("35f6", "heatseeker v2.01") -- https://github.com/tfausak/rattletrap/commit/57d86c2
  , ("372d", "a camera yaw attribute") -- https://github.com/tfausak/rattletrap/commit/9c1516c
  , ("383e", "older unknown content field") -- https://github.com/tfausak/rattletrap/pull/123
  , ("387f", "a frozen attribute") -- https://github.com/tfausak/rattletrap/commit/93ce196
  , ("3abd", "rlcs") -- https://github.com/tfausak/rattletrap/pull/86
  , ("3ea1", "a custom team name") -- https://github.com/tfausak/rattletrap/commit/cf4d145
  , ("4050", "v2.08 dodge impulse") -- https://github.com/tfausak/rattletrap/issues/247
  , ("4126", "a game mode after Neo Tokyo") -- https://github.com/tfausak/rattletrap/commit/a1cf21e
  , ("419a", "a club match") -- https://github.com/tfausak/rattletrap/commit/8e35043
  , ("42f0", "reservations after Neo Tokyo") -- https://github.com/tfausak/rattletrap/commit/163684f
  , ("42f2", "anniversary ball") -- https://github.com/tfausak/rattletrap/issues/147
  , ("43a9", "tutorial") -- https://github.com/nickbabcock/boxcars/pull/70
  , ("4bc3", "with timed out attribute") -- https://github.com/tfausak/rattletrap/pull/98
  , ("504e", "some messages") -- https://github.com/tfausak/rattletrap/commit/1d4a538
  , ("5123", "rep stat title") -- https://github.com/nickbabcock/boxcars/pull/78
  , ("520e", "no pickup attribute") -- https://github.com/tfausak/rattletrap/pull/38
  , ("524f", "quat edge case") -- https://github.com/tfausak/rattletrap/pull/87
  , ("52aa", "a match-ending attribute") -- https://github.com/tfausak/rattletrap/commit/5c64a6d
  , ("540d", "a demolish attribute") -- https://github.com/tfausak/rattletrap/commit/65ce033
  , ("54ae", "replicated car scale") -- https://github.com/nickbabcock/boxcars/pull/79
  , ("551c", "private match settings") -- https://github.com/tfausak/rattletrap/commit/5c9ebfc
  , ("5a06", "esports items") -- https://github.com/tfausak/rattletrap/pull/114
  , ("5e0b", "max channels") -- https://github.com/tfausak/rattletrap/issues/254
  , ("6210", "different player history key") -- https://github.com/tfausak/rattletrap/pull/63
  , ("6320", "a forfeit attribute") -- https://github.com/tfausak/rattletrap/pull/20
  , ("6688", "a malformed byte property") -- https://github.com/tfausak/rattletrap/commit/b1ec18b
  , ("6b0d", "patch 1.37") -- https://github.com/tfausak/rattletrap/pull/48
  , ("6d1b", "a flip right") -- https://github.com/tfausak/rattletrap/commit/ee7afa0
  , ("6f7c", "a map with numbers") -- https://github.com/tfausak/rattletrap/commit/2629511
  , ("7083", "weird basketball capitalization") -- https://github.com/tfausak/rattletrap/pull/63
  , ("7109", "a boost modifier") -- https://github.com/tfausak/rattletrap/commit/ee7afa0
  , ("7256", "special edition") -- https://github.com/tfausak/rattletrap/pull/103
  , ("7588", "another malformed byte property") -- https://github.com/nickbabcock/boxcars/pull/68
  , ("75ce", "primary and secondary titles") -- https://github.com/tfausak/rattletrap/pull/69
  , ("7bf6", "an online loadouts attribute") -- https://github.com/tfausak/rattletrap/commit/89d02f7
  , ("81d1", "gridiron") -- https://github.com/tfausak/rattletrap/pull/180
  , ("89cb", "remote user data") -- https://github.com/tfausak/rattletrap/commit/163684f
  , ("8ae5", "new painted items") -- https://github.com/tfausak/rattletrap/pull/43
  , ("92a6", "with server performance state") -- https://github.com/tfausak/rattletrap/pull/93
  , ("946f", "patch 1.43") -- https://github.com/tfausak/rattletrap/pull/69
  , ("9704", "a batarang") -- https://github.com/tfausak/rattletrap/commit/5958e5c
  , ("98e5", "a player using behind view") -- https://github.com/tfausak/rattletrap/commit/163684f
  , ("9a2c", "ghost hunt") -- https://github.com/tfausak/rattletrap/pull/160
  , ("9e35", "spike rush") -- https://github.com/tfausak/rattletrap/pull/160
  , ("9eaa", "newer replay without trailing bytes") -- https://github.com/tfausak/rattletrap/pull/126
  , ("a09e", "a tournament") -- https://github.com/tfausak/rattletrap/pull/69
  , ("a128", "a round count down") -- https://github.com/tfausak/rattletrap/commit/8bb778d
  , ("a184", "max score") -- https://github.com/tfausak/rattletrap/pull/158
  , ("a1c0", "epic system id") -- https://github.com/tfausak/rattletrap/pull/167
  , ("a52f", "some more mutators") -- https://github.com/tfausak/rattletrap/commit/ee7afa0
  , ("a558", "extended explosion data") -- https://github.com/tfausak/rattletrap/pull/44
  , ("a671", "a waiting player") -- https://github.com/tfausak/rattletrap/commit/163684f
  , ("a676", "new user color") -- https://github.com/tfausak/rattletrap/pull/93
  , ("a7f0", "a ready attribute") -- https://github.com/tfausak/rattletrap/commit/78af1fd
  , ("a9df", "salty shores patch 1.45") -- https://github.com/tfausak/rattletrap/pull/78
  , ("aa70", "patch 1.50 - TitleID attribute") -- https://github.com/tfausak/rattletrap/pull/93
  , ("ae46", "mvp") -- https://github.com/nickbabcock/boxcars/pull/80
  , ("afb1", "patch 1.37") -- https://github.com/tfausak/rattletrap/pull/48
  , ("b9f9", "a party leader") -- https://github.com/tfausak/rattletrap/commit/bba2cfd
  , ("c14f", "some mutators") -- https://github.com/tfausak/rattletrap/commit/bba2cfd
  , ("c23b", "new psynet id") -- https://github.com/tfausak/rattletrap/pull/118
  , ("c62c", "more boolean attributes") -- https://github.com/nickbabcock/boxcars/pull/77
  , ("c837", "a spectator") -- https://github.com/tfausak/rattletrap/commit/bba2cfd
  , ("cc4c", "after Starbase ARC") -- https://github.com/tfausak/rattletrap/pull/20
  , ("d044", "hoops mutators") -- https://github.com/tfausak/rattletrap/pull/34
  , ("d1d5", "v1.68") -- https://github.com/tfausak/rattletrap/pull/146
  , ("d236", "rlcs s2") -- https://github.com/tfausak/rattletrap/pull/88
  , ("d428", "a private hockey match") -- https://github.com/tfausak/rattletrap/commit/4c104b2
  , ("d44c", "ranked tournament") -- https://github.com/tfausak/rattletrap/pull/167
  , ("d52e", "psynet system id") -- https://github.com/tfausak/rattletrap/pull/99
  , ("d5d6", "health max") -- https://github.com/nickbabcock/boxcars/pull/80
  , ("d7fb", "an explosion attribute") -- https://github.com/tfausak/rattletrap/commit/c554e3e
  , ("d818", "heatseeker") -- https://github.com/tfausak/rattletrap/pull/160
  , ("db70", "new lag indicator") -- https://github.com/tfausak/rattletrap/pull/69
  , ("dcab", "weird ball attribute value") -- https://github.com/tfausak/rattletrap/issues/149
  , ("dcb3", "a pawn type attribute") -- https://github.com/tfausak/rattletrap/commit/7d7f438
  , ("dd14", "v1.88") -- https://github.com/tfausak/rattletrap/pull/170
  , ("de56", "a problematic product attribute") -- https://github.com/tfausak/rattletrap/issues/51
  , ("e2f9", "bTearOff") -- https://github.com/nickbabcock/boxcars/pull/76
  , ("e80d", "unlimited time") -- https://github.com/tfausak/rattletrap/pull/76
  , ("e978", "distracted") -- https://github.com/tfausak/rattletrap/issues/156
  , ("eae3", "an actor/object ID collision") -- https://github.com/tfausak/rattletrap/commit/d8fad06
  , ("eae8", "custom team colors") -- https://github.com/tfausak/rattletrap/commit/809240f
  , ("ecd5", "new match guid attribute") -- https://github.com/tfausak/rattletrap/issues/270
  , ("edbb", "remote role") -- https://github.com/tfausak/rattletrap/pull/106
  , ("f299", "a location attribute") -- https://github.com/tfausak/rattletrap/commit/21b09c5
  , ("f7b9", "a hockey game event") -- https://github.com/tfausak/rattletrap/commit/3e16d7f
  , ("f811", "no frames") -- https://github.com/tfausak/rattletrap/commit/bf4b6af
  , ("fdc7", "an MVP") -- https://github.com/tfausak/rattletrap/commit/65019d2
  , ("ffb7", "with difficulty") -- https://github.com/tfausak/rattletrap/pull/167
  , ("voice_update", "voice update replay") -- https://github.com/tfausak/rattletrap/pull/265
  ]
