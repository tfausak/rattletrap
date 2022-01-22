module Rattletrap.Type.AttributeValue where

import qualified Data.Map as Map
import qualified Rattletrap.BitGet as BitGet
import qualified Rattletrap.BitPut as BitPut
import qualified Rattletrap.Data as Data
import qualified Rattletrap.Exception.UnknownAttribute as UnknownAttribute
import qualified Rattletrap.Type.Attribute.AppliedDamage as AppliedDamage
import qualified Rattletrap.Type.Attribute.Boolean as Boolean
import qualified Rattletrap.Type.Attribute.Byte as Byte
import qualified Rattletrap.Type.Attribute.CamSettings as CamSettings
import qualified Rattletrap.Type.Attribute.ClubColors as ClubColors
import qualified Rattletrap.Type.Attribute.CustomDemolish as CustomDemolish
import qualified Rattletrap.Type.Attribute.DamageState as DamageState
import qualified Rattletrap.Type.Attribute.Demolish as Demolish
import qualified Rattletrap.Type.Attribute.Enum as Enum
import qualified Rattletrap.Type.Attribute.Explosion as Explosion
import qualified Rattletrap.Type.Attribute.ExtendedExplosion as ExtendedExplosion
import qualified Rattletrap.Type.Attribute.FlaggedByte as FlaggedByte
import qualified Rattletrap.Type.Attribute.FlaggedInt as FlaggedInt
import qualified Rattletrap.Type.Attribute.Float as Float
import qualified Rattletrap.Type.Attribute.GameMode as GameMode
import qualified Rattletrap.Type.Attribute.Int as Int
import qualified Rattletrap.Type.Attribute.Int64 as Int64
import qualified Rattletrap.Type.Attribute.Loadout as Loadout
import qualified Rattletrap.Type.Attribute.LoadoutOnline as LoadoutOnline
import qualified Rattletrap.Type.Attribute.Loadouts as Loadouts
import qualified Rattletrap.Type.Attribute.LoadoutsOnline as LoadoutsOnline
import qualified Rattletrap.Type.Attribute.Location as Location
import qualified Rattletrap.Type.Attribute.MusicStinger as MusicStinger
import qualified Rattletrap.Type.Attribute.PartyLeader as PartyLeader
import qualified Rattletrap.Type.Attribute.Pickup as Pickup
import qualified Rattletrap.Type.Attribute.PickupInfo as PickupInfo
import qualified Rattletrap.Type.Attribute.PickupNew as PickupNew
import qualified Rattletrap.Type.Attribute.PlayerHistoryKey as PlayerHistoryKey
import qualified Rattletrap.Type.Attribute.PrivateMatchSettings as PrivateMatchSettings
import qualified Rattletrap.Type.Attribute.QWord as QWord
import qualified Rattletrap.Type.Attribute.Reservation as Reservation
import qualified Rattletrap.Type.Attribute.RigidBodyState as RigidBodyState
import qualified Rattletrap.Type.Attribute.StatEvent as StatEvent
import qualified Rattletrap.Type.Attribute.String as String
import qualified Rattletrap.Type.Attribute.TeamPaint as TeamPaint
import qualified Rattletrap.Type.Attribute.Title as Title
import qualified Rattletrap.Type.Attribute.UniqueId as UniqueId
import qualified Rattletrap.Type.Attribute.WeldedInfo as WeldedInfo
import qualified Rattletrap.Type.AttributeType as AttributeType
import qualified Rattletrap.Type.Str as Str
import qualified Rattletrap.Type.U32 as U32
import qualified Rattletrap.Type.Version as Version
import qualified Rattletrap.Vendor.Argo as Argo

data AttributeValue
  = AppliedDamage AppliedDamage.AppliedDamage
  | Boolean Boolean.Boolean
  | Byte Byte.Byte
  | CamSettings CamSettings.CamSettings
  | ClubColors ClubColors.ClubColors
  | CustomDemolish CustomDemolish.CustomDemolish
  | DamageState DamageState.DamageState
  | Demolish Demolish.Demolish
  | Enum Enum.Enum
  | Explosion Explosion.Explosion
  | ExtendedExplosion ExtendedExplosion.ExtendedExplosion
  | FlaggedInt FlaggedInt.FlaggedInt
  | FlaggedByte FlaggedByte.FlaggedByte
  | Float Float.Float
  | GameMode GameMode.GameMode
  | Int Int.Int
  | Int64 Int64.Int64
  | Loadout Loadout.Loadout
  | LoadoutOnline LoadoutOnline.LoadoutOnline
  | Loadouts Loadouts.Loadouts
  | LoadoutsOnline LoadoutsOnline.LoadoutsOnline
  | Location Location.Location
  | MusicStinger MusicStinger.MusicStinger
  | PartyLeader PartyLeader.PartyLeader
  | Pickup Pickup.Pickup
  | PickupInfo PickupInfo.PickupInfo
  | PickupNew PickupNew.PickupNew
  | PlayerHistoryKey PlayerHistoryKey.PlayerHistoryKey
  | PrivateMatchSettings PrivateMatchSettings.PrivateMatchSettings
  | QWord QWord.QWord
  | Reservation Reservation.Reservation
  | RigidBodyState RigidBodyState.RigidBodyState
  | StatEvent StatEvent.StatEvent
  | String String.String
  | TeamPaint TeamPaint.TeamPaint
  | Title Title.Title
  | UniqueId UniqueId.UniqueId
  | WeldedInfo WeldedInfo.WeldedInfo
  deriving (Eq, Show)

instance Argo.HasCodec AttributeValue where
  codec =
    Argo.mapMaybe
        (Just . AppliedDamage)
        (\x -> case x of
          AppliedDamage y -> Just y
          _ -> Nothing
        )
        (Argo.fromObjectCodec
          Argo.Allow
          (Argo.required (Argo.fromString "applied_damage") Argo.codec)
        )
      Argo.<|> Argo.mapMaybe
                 (Just . Boolean)
                 (\x -> case x of
                   Boolean y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "boolean") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Byte)
                 (\x -> case x of
                   Byte y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "byte") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . CamSettings)
                 (\x -> case x of
                   CamSettings y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "cam_settings") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . ClubColors)
                 (\x -> case x of
                   ClubColors y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "club_colors") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . CustomDemolish)
                 (\x -> case x of
                   CustomDemolish y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required
                     (Argo.fromString "custom_demolish")
                     Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . DamageState)
                 (\x -> case x of
                   DamageState y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "damage_state") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Demolish)
                 (\x -> case x of
                   Demolish y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "demolish") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Enum)
                 (\x -> case x of
                   Enum y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "enum") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Explosion)
                 (\x -> case x of
                   Explosion y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "explosion") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . ExtendedExplosion)
                 (\x -> case x of
                   ExtendedExplosion y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required
                     (Argo.fromString "extended_explosion")
                     Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . FlaggedInt)
                 (\x -> case x of
                   FlaggedInt y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "flagged_int") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . FlaggedByte)
                 (\x -> case x of
                   FlaggedByte y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "flagged_byte") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Float)
                 (\x -> case x of
                   Float y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "float") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . GameMode)
                 (\x -> case x of
                   GameMode y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "game_mode") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Int)
                 (\x -> case x of
                   Int y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "int") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Int64)
                 (\x -> case x of
                   Int64 y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "int64") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Loadout)
                 (\x -> case x of
                   Loadout y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "loadout") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . LoadoutOnline)
                 (\x -> case x of
                   LoadoutOnline y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "loadout_online") Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Loadouts)
                 (\x -> case x of
                   Loadouts y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "loadouts") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . LoadoutsOnline)
                 (\x -> case x of
                   LoadoutsOnline y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required
                     (Argo.fromString "loadouts_online")
                     Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Location)
                 (\x -> case x of
                   Location y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "location") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . MusicStinger)
                 (\x -> case x of
                   MusicStinger y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "music_stinger") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . PartyLeader)
                 (\x -> case x of
                   PartyLeader y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "party_leader") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Pickup)
                 (\x -> case x of
                   Pickup y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "pickup") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . PickupInfo)
                 (\x -> case x of
                   PickupInfo y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "pickup_info") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . PickupNew)
                 (\x -> case x of
                   PickupNew y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "pickup_new") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . PlayerHistoryKey)
                 (\x -> case x of
                   PlayerHistoryKey y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required
                     (Argo.fromString "player_history_key")
                     Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . PrivateMatchSettings)
                 (\x -> case x of
                   PrivateMatchSettings y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required
                     (Argo.fromString "private_match_settings")
                     Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . QWord)
                 (\x -> case x of
                   QWord y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "q_word") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Reservation)
                 (\x -> case x of
                   Reservation y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "reservation") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . RigidBodyState)
                 (\x -> case x of
                   RigidBodyState y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required
                     (Argo.fromString "rigid_body_state")
                     Argo.codec
                   )
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . StatEvent)
                 (\x -> case x of
                   StatEvent y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "stat_event") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . String)
                 (\x -> case x of
                   String y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "string") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . TeamPaint)
                 (\x -> case x of
                   TeamPaint y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "team_paint") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . Title)
                 (\x -> case x of
                   Title y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "title") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . UniqueId)
                 (\x -> case x of
                   UniqueId y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "unique_id") Argo.codec)
                 )
      Argo.<|> Argo.mapMaybe
                 (Just . WeldedInfo)
                 (\x -> case x of
                   WeldedInfo y -> Just y
                   _ -> Nothing
                 )
                 (Argo.fromObjectCodec
                   Argo.Allow
                   (Argo.required (Argo.fromString "welded_info") Argo.codec)
                 )

bitPut :: AttributeValue -> BitPut.BitPut
bitPut value = case value of
  AppliedDamage x -> AppliedDamage.bitPut x
  Boolean x -> Boolean.bitPut x
  Byte x -> Byte.bitPut x
  CamSettings x -> CamSettings.bitPut x
  ClubColors x -> ClubColors.bitPut x
  CustomDemolish x -> CustomDemolish.bitPut x
  DamageState x -> DamageState.bitPut x
  Demolish x -> Demolish.bitPut x
  Enum x -> Enum.bitPut x
  Explosion x -> Explosion.bitPut x
  ExtendedExplosion x -> ExtendedExplosion.bitPut x
  FlaggedInt x -> FlaggedInt.bitPut x
  FlaggedByte x -> FlaggedByte.bitPut x
  Float x -> Float.bitPut x
  GameMode x -> GameMode.bitPut x
  Int x -> Int.bitPut x
  Int64 x -> Int64.putInt64Attribute x
  Loadout x -> Loadout.bitPut x
  LoadoutOnline x -> LoadoutOnline.bitPut x
  Loadouts x -> Loadouts.bitPut x
  LoadoutsOnline x -> LoadoutsOnline.bitPut x
  Location x -> Location.bitPut x
  MusicStinger x -> MusicStinger.bitPut x
  PartyLeader x -> PartyLeader.bitPut x
  Pickup x -> Pickup.bitPut x
  PickupInfo x -> PickupInfo.bitPut x
  PickupNew x -> PickupNew.bitPut x
  PlayerHistoryKey x -> PlayerHistoryKey.bitPut x
  PrivateMatchSettings x -> PrivateMatchSettings.bitPut x
  QWord x -> QWord.bitPut x
  Reservation x -> Reservation.bitPut x
  RigidBodyState x -> RigidBodyState.bitPut x
  StatEvent x -> StatEvent.bitPut x
  String x -> String.bitPut x
  TeamPaint x -> TeamPaint.bitPut x
  Title x -> Title.bitPut x
  UniqueId x -> UniqueId.bitPut x
  WeldedInfo x -> WeldedInfo.bitPut x

bitGet
  :: Version.Version
  -> Map.Map U32.U32 Str.Str
  -> Str.Str
  -> BitGet.BitGet AttributeValue
bitGet version objectMap name = BitGet.label "AttributeValue" $ do
  constructor <- case Map.lookup (Str.toText name) Data.attributeTypes of
    Nothing ->
      BitGet.throw . UnknownAttribute.UnknownAttribute $ Str.toString name
    Just x -> pure x
  case constructor of
    AttributeType.AppliedDamage ->
      fmap AppliedDamage $ AppliedDamage.bitGet version
    AttributeType.Boolean -> fmap Boolean Boolean.bitGet
    AttributeType.Byte -> fmap Byte Byte.bitGet
    AttributeType.CamSettings -> fmap CamSettings $ CamSettings.bitGet version
    AttributeType.ClubColors -> fmap ClubColors ClubColors.bitGet
    AttributeType.CustomDemolish ->
      fmap CustomDemolish $ CustomDemolish.bitGet version
    AttributeType.DamageState -> fmap DamageState $ DamageState.bitGet version
    AttributeType.Demolish -> fmap Demolish $ Demolish.bitGet version
    AttributeType.Enum -> fmap Enum Enum.bitGet
    AttributeType.Explosion -> fmap Explosion $ Explosion.bitGet version
    AttributeType.ExtendedExplosion ->
      fmap ExtendedExplosion $ ExtendedExplosion.bitGet version
    AttributeType.FlaggedInt -> fmap FlaggedInt FlaggedInt.bitGet
    AttributeType.FlaggedByte -> fmap FlaggedByte FlaggedByte.bitGet
    AttributeType.Float -> fmap Float Float.bitGet
    AttributeType.GameMode -> fmap GameMode $ GameMode.bitGet version
    AttributeType.Int -> fmap Int Int.bitGet
    AttributeType.Int64 -> fmap Int64 Int64.bitGet
    AttributeType.Loadout -> fmap Loadout Loadout.bitGet
    AttributeType.LoadoutOnline ->
      fmap LoadoutOnline $ LoadoutOnline.bitGet version objectMap
    AttributeType.Loadouts -> fmap Loadouts Loadouts.bitGet
    AttributeType.LoadoutsOnline ->
      fmap LoadoutsOnline $ LoadoutsOnline.bitGet version objectMap
    AttributeType.Location -> fmap Location $ Location.bitGet version
    AttributeType.MusicStinger -> fmap MusicStinger MusicStinger.bitGet
    AttributeType.PartyLeader -> fmap PartyLeader $ PartyLeader.bitGet version
    AttributeType.Pickup -> fmap Pickup Pickup.bitGet
    AttributeType.PickupInfo -> fmap PickupInfo PickupInfo.bitGet
    AttributeType.PickupNew -> fmap PickupNew PickupNew.bitGet
    AttributeType.PlayerHistoryKey ->
      fmap PlayerHistoryKey PlayerHistoryKey.bitGet
    AttributeType.PrivateMatchSettings ->
      fmap PrivateMatchSettings PrivateMatchSettings.bitGet
    AttributeType.QWord -> fmap QWord QWord.bitGet
    AttributeType.Reservation -> fmap Reservation $ Reservation.bitGet version
    AttributeType.RigidBodyState ->
      fmap RigidBodyState $ RigidBodyState.bitGet version
    AttributeType.StatEvent -> fmap StatEvent StatEvent.bitGet
    AttributeType.String -> fmap String String.bitGet
    AttributeType.TeamPaint -> fmap TeamPaint TeamPaint.bitGet
    AttributeType.Title -> fmap Title Title.bitGet
    AttributeType.UniqueId -> fmap UniqueId $ UniqueId.bitGet version
    AttributeType.WeldedInfo -> fmap WeldedInfo $ WeldedInfo.bitGet version
