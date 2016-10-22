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
spec = Hspec.describe "Rattletrap" (mapM_ (uncurry itCanGetAndPut) replays)

itCanGetAndPut :: String -> String -> Hspec.Spec
itCanGetAndPut uuid description =
  Hspec.it
    (uuid ++ ": a replay with " ++ description)
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
  [ ("00080014003600090000036E0F65CCEB", "a flip time")
  , ("07E925B1423653D44CB8B4B2524792C1", "a game mode before Neo Tokyo")
  , ("1205D96C4D819800927791820096CD49", "rumble mode")
  , ("160CA83E41083BFD8E6315B4BFCA0561", "a dedicated server IP")
  , ("18D6738D415B70B5BE4C299588D3C141", "an online loadout attribute")
  , ("1A126AC24CAA0DB0E98835BD960B8AF8", "overtime")
  , ("1BC2D01444ACE577D01E988EADD4DFD0", "no padding after the frames")
  , ("1D1DE97D4941C86E43FE0093563DB621", "a camera pitch")
  , ("211466D04B983F5A33CC2FA1D5928672", "a match save")
  , ("22BACD794ABE7B92E50E9CBDBD9C59CE", "a vote to forfeit")
  , ("29F582C34A65EB34D358A784CBE3C189", "frames")
  , ("2F817C8C44859C762980AE85B1862A31", "splitscreen players")
  , ("372DBFCA4BDB340E4357B6BD43032802", "a camera yaw attribute")
  , ("387F059C47C09E253C875CA990EFD9F2", "a frozen attribute")
  , ("3EA147DD485B8DD24810689A7A989E44", "a custom team name")
  , ("4126861E477F4A03DE2A4080374D7908", "a game mode after Neo Tokyo")
  , ("42F0D8DA4FC89AE7B80FCAB7F637A8EA", "reservations after Neo Tokyo")
  , ("52AA67F94090C19D33C5009E54D31FE4", "a match-ending attribute")
  , ("540DA764423C8FB24EB9D486D982F16F", "a demolish attribute")
  , ("551CA4D44FF2B86015DE44A6B5790D4C", "private match settings")
  , ("6688EEE34BFEB3EC3A9E3283098CC712", "a malformed byte property")
  , ("6D1B06D844A5BB91B81FD4B5B28F08BA", "a flip right")
  , ("7109EB9846D303E54B7ACBA792036213", "a boost modifier")
  , ("7BF6073F4614CE0A438994B9A260DA6A", "an online loadouts attribute")
  , ("89CBA30E46FA5385BDD35DA4285D4D2E", "remote user data")
  , ("A52F804845573D8DA65E97BF59026A43", "some more mutators")
  , ("A6711CE74272B2E663DCC9A200A218E3", "a waiting player")
  , ("A7F001A1417A19BFA8C90990D8F7C2FF", "a ready attribute")
  , ("B9F9B87D4A9D0A3D25D4EC91C0401DE2", "a party leader")
  , ("C14F7E0E4D9B5E6BE9AD5D8ED56B174C", "some mutators")
  , ("C375E0EC4971B506C51678B465D35AE9", "some UTF-16 text")
  , ("C80A9959479CFE51673B3A889D717554", "some Latin-1 text")
  , ("C8372B1345B1803DEF039F815DBD802D", "a spectator")
  , ("CB5B422A4424262B2877E9A121329EF2", "a player using behind view")
  , ("D7FB197A451D69075A0C99A2F49A4053", "an explosion attribute")
  , ("DCB3A6B94A9DBE46FDE5EAA9B012F6C8", "a pawn type attribute")
  , ("EAE311E84BA35B590A6FDBA6DD4F2FEB", "an actor/object ID collision")
  , ("F299F176491554B11E34AB91CA76B2CE", "a location attribute")
  , ("F811C1D24888015E23B598AD8628C742", "no frames")
  , ("FDC79DA84DD463D4BCCE6B892829AC88", "an MVP")
  ]
