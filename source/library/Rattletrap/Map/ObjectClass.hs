module Rattletrap.Map.ObjectClass where

import Rattletrap.Data
import Rattletrap.Map.Class
import Rattletrap.Map.Object
import Rattletrap.Primitive

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Word as Word

newtype ObjectClassMap = ObjectClassMap
  { objectClassMapValue :: HashMap.HashMap Word.Word32 Word32
  } deriving (Eq, Show)

makeObjectClassMap :: ObjectMap -> ClassMap -> ObjectClassMap
makeObjectClassMap objectMap classMap = do
  let objectIds = objectMapKeys objectMap
  let classIds = map (getClassId objectMap classMap) objectIds
  let rawPairs = zip objectIds classIds
  let pairs =
        Maybe.mapMaybe
          (\(objectId, maybeClassId) ->
             case maybeClassId of
               Nothing -> Nothing
               Just classId -> Just (word32Value objectId, classId))
          rawPairs
  ObjectClassMap (HashMap.fromList pairs)

getClassId :: ObjectMap -> ClassMap -> Word32 -> Maybe Word32
getClassId objectMap classMap objectId = do
  objectName <- objectMapLookup objectId objectMap
  className <- getClassName objectName
  classMapLookupR className classMap

getClassName :: Text -> Maybe Text
getClassName rawObjectName =
  HashMap.lookup (normalizeObjectName rawObjectName) objectClasses

normalizeObjectName :: Text -> Text.Text
normalizeObjectName objectName =
  let name = textValue objectName
      crowdActor = Text.pack "TheWorld:PersistentLevel.CrowdActor_TA"
      crowdManager = Text.pack "TheWorld:PersistentLevel.CrowdManager_TA"
      boostPickup = Text.pack "TheWorld:PersistentLevel.VehiclePickup_Boost_TA"
      mapScoreboard = Text.pack "TheWorld:PersistentLevel.InMapScoreboard_TA"
  in if Text.isInfixOf crowdActor name
       then crowdActor
       else if Text.isInfixOf crowdManager name
              then crowdManager
              else if Text.isInfixOf boostPickup name
                     then boostPickup
                     else if Text.isInfixOf mapScoreboard name
                            then mapScoreboard
                            else name

objectClasses :: HashMap.HashMap Text.Text Text
objectClasses =
  HashMap.fromList
    (map (\(k, v) -> (Text.pack k, stringToText v)) rawObjectClasses)

objectClassMapLookup :: Word32 -> ObjectClassMap -> Maybe Word32
objectClassMapLookup objectId (ObjectClassMap objectClassMap) =
  HashMap.lookup (word32Value objectId) objectClassMap
