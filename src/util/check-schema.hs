import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  entries <- Directory.listDirectory output
  Process.callCommand
    . unwords
    $ "npx"
    : "ajv"
    -- : "-s"
    : FilePath.combine output schema
    : concatMap
        (\entry -> if isJson entry && not (isSchema entry)
          then ["-d", FilePath.combine output entry]
          else []
        )
        (List.sort entries)

output :: FilePath
output = "output"

isJson :: FilePath -> Bool
isJson = FilePath.isExtensionOf "json"

isSchema :: FilePath -> Bool
isSchema = (== schema) . FilePath.takeFileName

schema :: FilePath
schema = "schema.json"
