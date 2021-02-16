module Rattletrap.Console.Mode where

data Mode
  = Decode
  | Encode
  deriving (Eq, Show)

fromString :: String -> Either String Mode
fromString string = case string of
  "decode" -> Right Decode
  "encode" -> Right Encode
  _ -> Left $ "invalid mode: " <> show string
