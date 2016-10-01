module Rattletrap.ClassPropertyMap where

import Rattletrap.Cache
import Rattletrap.ClassMapping
import Rattletrap.CompressedWord
import Rattletrap.List
import Rattletrap.Text
import Rattletrap.Word32

import qualified Data.Set as Set

data ClassPropertyMap = ClassPropertyMap
  {
  } deriving (Eq, Ord, Show)

makeClassPropertyMap :: List Text
                     -> List ClassMapping
                     -> List Cache
                     -> ClassPropertyMap
makeClassPropertyMap _objects _classMappings _caches = ClassPropertyMap {}

getClassName :: ClassPropertyMap -> Word32 -> Text
getClassName _classPropertyMap _objectId = error "getClassName"

classHasLocation :: Text -> Bool
classHasLocation className = Set.member className classesWithLocation

classesWithLocation :: Set.Set Text
classesWithLocation =
  Set.fromList
    (map
       stringToText
       [ "TAGame.Ball_TA"
       , "TAGame.CameraSettingsActor_TA"
       , "TAGame.Car_Season_TA"
       , "TAGame.Car_TA"
       , "TAGame.CarComponent_Boost_TA"
       , "TAGame.CarComponent_Dodge_TA"
       , "TAGame.CarComponent_DoubleJump_TA"
       , "TAGame.CarComponent_FlipCar_TA"
       , "TAGame.CarComponent_Jump_TA"
       , "TAGame.GameEvent_Season_TA"
       , "TAGame.GameEvent_Soccar_TA"
       , "TAGame.GameEvent_SoccarPrivate_TA"
       , "TAGame.GameEvent_SoccarSplitscreen_TA"
       , "TAGame.GRI_TA"
       , "TAGame.PRI_TA"
       , "TAGame.SpecialPickup_BallCarSpring_TA"
       , "TAGame.SpecialPickup_BallFreeze_TA"
       , "TAGame.SpecialPickup_BallGravity_TA"
       , "TAGame.SpecialPickup_BallGravity_TA"
       , "TAGame.SpecialPickup_BallLasso_TA"
       , "TAGame.SpecialPickup_BallVelcro_TA"
       , "TAGame.SpecialPickup_Batarang_TA"
       , "TAGame.SpecialPickup_BoostOverride_TA"
       , "TAGame.SpecialPickup_GrapplingHook_TA"
       , "TAGame.SpecialPickup_HitForce_TA"
       , "TAGame.SpecialPickup_Swapper_TA"
       , "TAGame.SpecialPickup_Tornado_TA"
       , "TAGame.Team_Soccar_TA"
       ])

classHasRotation :: Text -> Bool
classHasRotation className = Set.member className classesWithRotation

classesWithRotation :: Set.Set Text
classesWithRotation =
  Set.fromList
    (map
       stringToText
       ["TAGame.Ball_TA", "TAGame.Car_Season_TA", "TAGame.Car_TA"])

attributeIdLimit :: ClassPropertyMap -> CompressedWord -> Word
attributeIdLimit _classPropertyMap _actorId = error "attributeIdLimit"

getAttributeName :: ClassPropertyMap -> CompressedWord -> CompressedWord -> Text
getAttributeName _classPropertyMap _actorId _attributeId =
  error "getAttributeName"
