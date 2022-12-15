module Rattletrap.Type.Attribute.GameServer where

import qualified Data.Foldable as Foldable
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Schema as Schema
import qualified Rattletrap.Type.Attribute.QWord as QWord
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Utility.Json as Json

data GameServer
  = Old QWord.QWord
  | New Str.Str
  deriving (Eq, Show)

instance Json.FromJSON GameServer where
  parseJSON = Json.withObject "GameServer" $ \x -> Foldable.asum
    [ fmap Old $ Json.required x "old"
    , fmap New $ Json.required x "new"
    ]

instance Json.ToJSON GameServer where
  toJSON x = case x of
    Old y -> Json.object [Json.pair "old" $ Json.toJSON y]
    New y -> Json.object [Json.pair "new" $ Json.toJSON y]

schema :: Schema.Schema
schema = Schema.named "attribute-game-server" . Schema.oneOf $ fmap
  (\(k, v) -> Schema.object [(Json.pair k v, True)])
  [ ("old", Schema.ref QWord.schema)
  , ("new", Schema.ref Str.schema)
  ]

bitPut :: GameServer -> BitPut.BitPut
bitPut x = case x of
  Old y -> QWord.bitPut y
  New y -> Str.bitPut y

bitGet :: Maybe Str.Str -> BitGet.BitGet GameServer
bitGet buildVersion = BitGet.label "GameServer" $
  if buildVersion >= Just (Str.fromString "221120.42953.406184")
    then BitGet.label "New" $ fmap New Str.bitGet
    else BitGet.label "Old" $ fmap Old QWord.bitGet
