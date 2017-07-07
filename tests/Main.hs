import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as ByteString
import qualified Rattletrap
import qualified System.FilePath as FilePath
import qualified System.IO.Temp as Temp
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec spec

spec :: Hspec.Spec
spec = Hspec.describe "Rattletrap" (mapM_ (uncurry itCanRoundTrip) replays)

itCanRoundTrip :: String -> String -> Hspec.Spec
itCanRoundTrip uuid description =
  Hspec.it (specName uuid description) (specBody uuid)

specName :: String -> String -> String
specName uuid description = unwords [take 4 uuid, description]

specBody :: String -> IO ()
specBody uuid = do
  let inputFile = pathToReplay uuid
  input <- ByteString.readFile inputFile
  Temp.withSystemTempDirectory
    "replay-"
    (\directory -> do
       let jsonFile = FilePath.combine directory "replay.json"
       Rattletrap.mainWithArgs ["decode", inputFile, jsonFile]
       let outputFile = FilePath.combine directory "output.replay"
       Rattletrap.mainWithArgs ["encode", jsonFile, outputFile]
       output <- ByteString.readFile outputFile
       Monad.unless
         (output == input)
         (Hspec.expectationFailure "output does not match input"))

pathToReplay :: String -> FilePath
pathToReplay uuid =
  FilePath.joinPath ["replays", FilePath.addExtension uuid ".replay"]

replays :: [(String, String)]
replays =
  [ ("00080014003600090000036E0F65CCEB", "a flip time")
  , ("07E925B1423653D44CB8B4B2524792C1", "a game mode before Neo Tokyo")
  , ("0AD21FEE4A512906BEB6C98136AAF49A", "some Latin-1 text")
  , ("1205D96C4D819800927791820096CD49", "rumble mode")
  , ("160CA83E41083BFD8E6315B4BFCA0561", "a dedicated server IP")
  , ("16D580EF479483E015207C901776F9FB", "new property types")
  , ("18D6738D415B70B5BE4C299588D3C141", "an online loadout attribute")
  , ("1A126AC24CAA0DB0E98835BD960B8AF8", "overtime")
  , ("1AE415514DFC65DCBF8B8391AD35488D", "a game time")
  , ("1BC2D01444ACE577D01E988EADD4DFD0", "no padding after the frames")
  , ("1D1DE97D4941C86E43FE0093563DB621", "a camera pitch")
  , ("1EF90FCC4F719F606A5327B3CDD782A4", "a private hoops match")
  , ("1F3798E540B0C37A946561ABBB3037F9", "splitscreen players")
  , ("211466D04B983F5A33CC2FA1D5928672", "a match save")
  , ("22660E3649FC7971E5653692473D4318", "dropshot")
  , ("22BACD794ABE7B92E50E9CBDBD9C59CE", "a vote to forfeit")
  , ("27B6A7B64553F0F685874584F96BAB1B", "some UTF-16 text")
  , ("29F582C34A65EB34D358A784CBE3C189", "frames")
  , ("372DBFCA4BDB340E4357B6BD43032802", "a camera yaw attribute")
  , ("387F059C47C09E253C875CA990EFD9F2", "a frozen attribute")
  , ("3EA147DD485B8DD24810689A7A989E44", "a custom team name")
  , ("4126861E477F4A03DE2A4080374D7908", "a game mode after Neo Tokyo")
  , ("42F0D8DA4FC89AE7B80FCAB7F637A8EA", "reservations after Neo Tokyo")
  , ("504ED825482186E771FAA9B642CE5CE4", "some messages")
  , ("520E1BFF468CF6C3C48D1EA85D9C7909", "no pickup attribute")
  , ("52AA67F94090C19D33C5009E54D31FE4", "a match-ending attribute")
  , ("540DA764423C8FB24EB9D486D982F16F", "a demolish attribute")
  , ("551CA4D44FF2B86015DE44A6B5790D4C", "private match settings")
  , ("6320E51C49066A7C210A2993C2201D5F", "a forfeit attribute")
  , ("6688EEE34BFEB3EC3A9E3283098CC712", "a malformed byte property")
  , ("6D1B06D844A5BB91B81FD4B5B28F08BA", "a flip right")
  , ("6F7CFCD24638F8A6567AB3A8B9958A90", "a map with numbers")
  , ("7109EB9846D303E54B7ACBA792036213", "a boost modifier")
  , ("7BF6073F4614CE0A438994B9A260DA6A", "an online loadouts attribute")
  , ("89CBA30E46FA5385BDD35DA4285D4D2E", "remote user data")
  , ("9704208245D7DD851F2FB2BC7DFD9AC3", "a batarang")
  , ("98E58A904D713F2DE202358E8573265D", "a player using behind view")
  , ("A128B3AB45D5A18E3EF9CF93C9576BCE", "a round count down")
  , ("A52F804845573D8DA65E97BF59026A43", "some more mutators")
  , ("A6711CE74272B2E663DCC9A200A218E3", "a waiting player")
  , ("A7F001A1417A19BFA8C90990D8F7C2FF", "a ready attribute")
  , ("B9F9B87D4A9D0A3D25D4EC91C0401DE2", "a party leader")
  , ("C14F7E0E4D9B5E6BE9AD5D8ED56B174C", "some mutators")
  , ("C8372B1345B1803DEF039F815DBD802D", "a spectator")
  , ("CC4CA70D4F7A67EBAD0ED9B9923106F7", "after Starbase ARC")
  , ("D0449F5F4AA775B86FFA7DA2B5A3204E", "hoops mutators")
  , ("D428F81646A98C25902CE988AE5C14C8", "a private hockey match")
  , ("D7FB197A451D69075A0C99A2F49A4053", "an explosion attribute")
  , ("DCB3A6B94A9DBE46FDE5EAA9B012F6C8", "a pawn type attribute")
  , ("EAE311E84BA35B590A6FDBA6DD4F2FEB", "an actor/object ID collision")
  , ("EAE8DADA4BB2DC5422792C9B4A67392D", "custom team colors")
  , ("F299F176491554B11E34AB91CA76B2CE", "a location attribute")
  , ("F7B9E14545C7467B89A00895980FCD73", "a hockey game event")
  , ("F811C1D24888015E23B598AD8628C742", "no frames")
  , ("FDC79DA84DD463D4BCCE6B892829AC88", "an MVP")
  ]
