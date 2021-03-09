import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  entries <- Directory.listDirectory output
  Monad.forM_ (List.sort entries) $ \entry -> do
    Monad.when (isJson entry && not (isSchema entry)) $ do
      ajv (FilePath.combine output schema) (FilePath.combine output entry)

output :: FilePath
output = "output"

isJson :: FilePath -> Bool
isJson = FilePath.isExtensionOf "json"

isSchema :: FilePath -> Bool
isSchema = (== schema) . FilePath.takeFileName

schema :: FilePath
schema = "schema.json"

ajv :: FilePath -> FilePath -> IO ()
ajv s d = Process.callCommand $ unwords ["npx", "ajv", "-s", s, "-d", d]
