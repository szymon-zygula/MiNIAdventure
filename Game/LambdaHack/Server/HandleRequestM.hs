{-# LANGUAGE GADTs #-}
-- | Semantics of request.
-- A couple of them do not take time, the rest does.
-- Note that since the results are atomic commands, which are executed
-- only later (on the server and some of the clients), all condition
-- are checkd by the semantic functions in the context of the state
-- before the server command. Even if one or more atomic actions
-- are already issued by the point an expression is evaluated, they do not
-- influence the outcome of the evaluation.
-- TODO: document
module Game.LambdaHack.Server.HandleRequestM
  ( handleRequestAI, handleRequestUI, switchLeader, handleRequestTimed
  , reqMove, reqDisplace, reqGameExit
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , setBWait, handleRequestTimedCases
  , affectSmell, reqMelee, reqAlter, reqWait
  , reqMoveItems, reqMoveItem, computeRndTimeout, reqProject, reqApply
  , reqGameRestart, reqGameSave, reqTactic, reqAutomate
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Atomic
import qualified Game.LambdaHack.Common.Ability as Ability
import Game.LambdaHack.Common.Actor
import Game.LambdaHack.Common.ActorState
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Item
import qualified Game.LambdaHack.Common.Kind as Kind
import Game.LambdaHack.Common.Level
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.MonadStateRead
import Game.LambdaHack.Common.Point
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Common.Request
import Game.LambdaHack.Common.State
import qualified Game.LambdaHack.Common.Tile as Tile
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Common.Vector
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.ModeKind
import qualified Game.LambdaHack.Content.TileKind as TK
import Game.LambdaHack.Server.CommonM
import Game.LambdaHack.Server.HandleEffectM
import Game.LambdaHack.Server.ItemM
import Game.LambdaHack.Server.MonadServer
import Game.LambdaHack.Server.PeriodicM
import Game.LambdaHack.Server.State

-- | The semantics of server commands.
-- AI always takes time and so doesn't loop.
handleRequestAI :: (MonadAtomic m)
                => ReqAI
                -> m (Maybe RequestAnyAbility)
{-# INLINABLE handleRequestAI #-}
handleRequestAI cmd = case cmd of
  ReqAITimed cmdT -> return $ Just cmdT
  ReqAINop -> return Nothing

-- | The semantics of server commands. Only the first two cases take time.
handleRequestUI :: (MonadAtomic m, MonadServer m)
                => FactionId -> ActorId -> ReqUI
                -> m (Maybe RequestAnyAbility)
{-# INLINABLE handleRequestUI #-}
handleRequestUI fid aid cmd = case cmd of
  ReqUITimed cmdT -> return $ Just cmdT
  ReqUIGameRestart t d names -> reqGameRestart aid t d names >> return Nothing
  ReqUIGameExit -> reqGameExit aid >> return Nothing
  ReqUIGameSave -> reqGameSave >> return Nothing
  ReqUITactic toT -> reqTactic fid toT >> return Nothing
  ReqUIAutomate -> reqAutomate fid >> return Nothing
  ReqUINop -> return Nothing

setBWait :: (MonadAtomic m) => RequestTimed a -> ActorId -> m Bool
{-# INLINE setBWait #-}
setBWait cmd aidNew = do
  let hasReqWait ReqWait{} = True
      hasReqWait _ = False
      hasWait = hasReqWait cmd
  bPre <- getsState $ getActorBody aidNew
  when (hasWait /= bwait bPre) $
    execUpdAtomic $ UpdWaitActor aidNew hasWait
  return hasWait

handleRequestTimed :: (MonadAtomic m, MonadServer m)
                   => FactionId -> ActorId -> RequestTimed a -> m Bool
{-# INLINABLE handleRequestTimed #-}
handleRequestTimed fid aid cmd = do
  hasWait <- setBWait cmd aid
  unless hasWait $ overheadActorTime fid
  advanceTime aid
  handleRequestTimedCases aid cmd
  managePerTurn aid
  return $ not hasWait

handleRequestTimedCases :: (MonadAtomic m, MonadServer m)
                        => ActorId -> RequestTimed a -> m ()
{-# INLINABLE handleRequestTimedCases #-}
handleRequestTimedCases aid cmd = case cmd of
  ReqMove target -> reqMove aid target
  ReqMelee target iid cstore -> reqMelee aid target iid cstore
  ReqDisplace target -> reqDisplace aid target
  ReqAlter tpos mfeat -> reqAlter aid tpos mfeat
  ReqWait -> reqWait aid
  ReqMoveItems l -> reqMoveItems aid l
  ReqProject p eps iid cstore -> reqProject aid p eps iid cstore
  ReqApply iid cstore -> reqApply aid iid cstore

switchLeader :: (MonadAtomic m, MonadServer m)
             => FactionId -> ActorId -> m ()
{-# INLINE switchLeader #-}
switchLeader fid aidNew = do
  fact <- getsState $ (EM.! fid) . sfactionD
  bPre <- getsState $ getActorBody aidNew
  let mleader = gleader fact
      actorChanged = mleader /= Just aidNew
  let !_A = assert (Just aidNew /= mleader
                    && not (bproj bPre)
                    `blame` (aidNew, bPre, fid, fact)) ()
  let !_A = assert (bfid bPre == fid
                    `blame` "client tries to move other faction actors"
                    `twith` (aidNew, bPre, fid, fact)) ()
  let (autoDun, _) = autoDungeonLevel fact
  arena <- case mleader of
    Nothing -> return $! blid bPre
    Just leader -> do
      b <- getsState $ getActorBody leader
      return $! blid b
  if | actorChanged && blid bPre /= arena && autoDun ->
       execFailure aidNew ReqWait{-hack-} NoChangeDunLeader
     | otherwise -> do
       execUpdAtomic $ UpdLeadFaction fid mleader (Just aidNew)
     -- We exchange times of the old and new leader.
     -- This permits an abuse, because a slow tank can be moved fast
     -- by alternating between it and many fast actors (until all of them
     -- get slowed down by this and none remain). But at least the sum
     -- of all times of a faction is conserved. And we avoid double moves
     -- against the UI player caused by his leader changes. There may still
     -- happen double moves caused by AI leader changes, but that's rare.
     -- The flip side is the possibility of multi-moves of the UI player
     -- as in the case of the tank.
     -- Warning: when the action is performed on the server,
     -- the time of the actor is different than when client prepared that
     -- action, so any client checks involving time should discount this.
       case mleader of
         Just aidOld | aidOld /= aidNew -> swapTime aidOld aidNew
         _ -> return ()

-- * ReqMove

-- | Add a smell trace for the actor to the level. For now, only actors
-- with gender leave strong and unique enough smell. If smell already there
-- and the actor can smell, remove smell. Projectiles are ignored.
-- As long as an actor can smell, he doesn't leave any smell ever.
affectSmell :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
{-# INLINABLE affectSmell #-}
affectSmell aid = do
  b <- getsState $ getActorBody aid
  unless (bproj b) $ do
    fact <- getsState $ (EM.! bfid b) . sfactionD
    actorAspect <- getsServer sactorAspect
    let ar = actorAspect EM.! aid
        smellRadius = aSmell ar
    when (fhasGender (gplayer fact) || smellRadius > 0) $ do
      localTime <- getsState $ getLocalTime $ blid b
      lvl <- getLevel $ blid b
      let oldS = fromMaybe timeZero $ EM.lookup (bpos b) . lsmell $ lvl
          newTime = timeShift localTime smellTimeout
          newS = if smellRadius > 0
                 then timeZero
                 else newTime
      when (oldS /= newS) $
        execUpdAtomic $ UpdAlterSmell (blid b) (bpos b) oldS newS

-- | Actor moves or attacks.
-- Note that client may not be able to see an invisible monster
-- so it's the server that determines if melee took place, etc.
-- Also, only the server is authorized to check if a move is legal
-- and it needs full context for that, e.g., the initial actor position
-- to check if melee attack does not try to reach to a distant tile.
reqMove :: (MonadAtomic m, MonadServer m) => ActorId -> Vector -> m ()
{-# INLINABLE reqMove #-}
reqMove source dir = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  let lid = blid sb
  lvl <- getLevel lid
  let spos = bpos sb           -- source position
      tpos = spos `shift` dir  -- target position
  -- We start by checking actors at the target position.
  tgt <- getsState $ posToAssocs tpos lid
  case tgt of
    (target, tb) : _ | not (bproj sb && bproj tb) -> do  -- visible or not
      -- Projectiles are too small to hit each other.
      -- Attacking does not require full access, adjacency is enough.
      -- Here the only weapon of projectiles is picked, too.
      mweapon <- pickWeaponServer source
      case mweapon of
        Nothing -> reqWait source
        Just (wp, cstore) -> reqMelee source target wp cstore
    _
      | accessible cops lvl tpos -> do
          -- Movement requires full access.
          execUpdAtomic $ UpdMoveActor source spos tpos
          affectSmell source
      | otherwise ->
          -- Client foolishly tries to move into blocked, boring tile.
          execFailure source (ReqMove dir) MoveNothing

-- * ReqMelee

-- | Resolves the result of an actor moving into another.
-- Actors on blocked positions can be attacked without any restrictions.
-- For instance, an actor embedded in a wall can be attacked from
-- an adjacent position. This function is analogous to projectGroupItem,
-- but for melee and not using up the weapon.
-- No problem if there are many projectiles at the spot. We just
-- attack the one specified.
reqMelee :: (MonadAtomic m, MonadServer m)
         => ActorId -> ActorId -> ItemId -> CStore -> m ()
{-# INLINABLE reqMelee #-}
reqMelee source target iid cstore = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
      hpMax = aMaxHP ar
      adj = checkAdjacent sb tb
      req = ReqMelee target iid cstore
  if source == target then execFailure source req MeleeSelf
  else if not adj then execFailure source req MeleeDistant
  else do
    let sfid = bfid sb
        tfid = bfid tb
    sfact <- getsState $ (EM.! sfid) . sfactionD
    hurtBonus <- armorHurtBonus source target
    itemBase <- getsState $ getItemBody iid
    n <- rndToAction $ castDice (AbsDepth 0) (AbsDepth 0) $ jdamage itemBase
    let mult = 100 + hurtBonus
        rawDeltaHP | n <= 0 = 0
                   | otherwise = - (max oneM  -- at least 1 HP taken
                                        (fromIntegral mult * xM n `divUp` 100))
        serious = rawDeltaHP < 0 && source /= target && not (bproj tb)
        deltaHP | serious = -- if HP overfull, at least cut back to max HP
                            min rawDeltaHP (xM hpMax - bhp tb)
                | otherwise = rawDeltaHP
    -- Damage the target.
    when (rawDeltaHP < 0) $ do
      execUpdAtomic $ UpdRefillHP target deltaHP
      when serious $ halveCalm target
      let hitA | hurtBonus <= -50  -- e.g., braced and no hit bonus
                 = HitBlock 2
               | hurtBonus <= -10  -- low bonus vs armor
                 = HitBlock 1
               | otherwise = HitClear
      execSfxAtomic $ SfxStrike source target iid cstore hitA
    -- Deduct a hitpoint for a pierce of a projectile
    -- or due to a hurled actor colliding with another or a wall.
    case btrajectory sb of
      Nothing -> return ()
      Just (tra, speed) -> do
        execUpdAtomic $ UpdRefillHP source minusM
        unless (bproj sb || null tra) $
          -- Non-projectiles can't pierce, so terminate their flight.
          execUpdAtomic
          $ UpdTrajectory source (btrajectory sb) (Just ([], speed))
    let c = CActor source cstore
    -- Msgs inside itemEffect describe the target part.
    itemEffectAndDestroy source target iid c
    -- The only way to start a war is to slap an enemy. Being hit by
    -- and hitting projectiles count as unintentional friendly fire.
    let friendlyFire = bproj sb || bproj tb
        fromDipl = EM.findWithDefault Unknown tfid (gdipl sfact)
    unless (friendlyFire
            || isAtWar sfact tfid  -- already at war
            || isAllied sfact tfid  -- allies never at war
            || sfid == tfid) $
      execUpdAtomic $ UpdDiplFaction sfid tfid fromDipl War

armorHurtBonus :: (MonadAtomic m, MonadServer m)
               => ActorId -> ActorId
               -> m Int
{-# INLINABLE armorHurtBonus #-}
armorHurtBonus source target = do
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  actorAspect <- getsServer sactorAspect
  let trim200 n = min 200 $ max (-200) n
      block200 n = min 200 $ max (-200) $ n + if braced tb then 50 else 0
      sar = actorAspect EM.! source
      tar = actorAspect EM.! target
      itemBonus = if bproj sb
                  then trim200 (aHurtRanged sar) - block200 (aArmorRanged tar)
                  else trim200 (aHurtMelee sar) - block200 (aArmorMelee tar)
  return $! min 100 $ max (-100) itemBonus

-- * ReqDisplace

-- | Actor tries to swap positions with another.
reqDisplace :: (MonadAtomic m, MonadServer m) => ActorId -> ActorId -> m ()
{-# INLINABLE reqDisplace #-}
reqDisplace source target = do
  cops <- getsState scops
  sb <- getsState $ getActorBody source
  tb <- getsState $ getActorBody target
  tfact <- getsState $ (EM.! bfid tb) . sfactionD
  let tpos = bpos tb
      adj = checkAdjacent sb tb
      atWar = isAtWar tfact (bfid sb)
      req = ReqDisplace target
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! target
  dEnemy <- getsState $ dispEnemy source target $ aSkills ar
  if | not adj -> execFailure source req DisplaceDistant
     | atWar && not dEnemy -> do
       mweapon <- pickWeaponServer source
       case mweapon of
         Nothing -> reqWait source
         Just (wp, cstore)  -> reqMelee source target wp cstore
           -- DisplaceDying, etc.
     | otherwise -> do
       let lid = blid sb
       lvl <- getLevel lid
       -- Displacing requires full access.
       if accessible cops lvl tpos then
         case posToAidsLvl tpos lvl of
           [] -> assert `failure` (source, sb, target, tb)
           [_] -> execUpdAtomic $ UpdDisplaceActor source target
           _ -> execFailure source req DisplaceProjectiles
       else
         -- Client foolishly tries to displace an actor without access.
         execFailure source req DisplaceAccess

-- * ReqAlter

-- | Search and/or alter the tile.
--
-- Note that if @serverTile /= freshClientTile@, @freshClientTile@
-- should not be alterable (but @serverTile@ may be).
reqAlter :: (MonadAtomic m, MonadServer m)
         => ActorId -> Point -> Maybe TK.Feature -> m ()
{-# INLINABLE reqAlter #-}
reqAlter source tpos mfeat = do
  cops@Kind.COps{cotile=cotile@Kind.Ops{okind, opick}, coTileSpeedup} <- getsState scops
  sb <- getsState $ getActorBody source
  actorSk <- actorSkillsServer source
  let alterSkill = EM.findWithDefault 0 Ability.AbAlter actorSk
      lid = blid sb
      spos = bpos sb
      req = ReqAlter tpos mfeat
  lvl <- getLevel lid
  let serverTile = lvl `at` tpos
      freshClientTile = hideTile cops lvl tpos
  -- Only actors with AbAlter > 1 can search for hidden doors, etc.
  if alterSkill <= 1
     || serverTile == freshClientTile  -- no searching needed
        && alterSkill < Tile.alterMinSkill coTileSpeedup serverTile
  then execFailure source req AlterUnskilled
  else if not $ adjacent spos tpos then execFailure source req AlterDistant
  else do
    let changeTo tgroup = do
          -- No @SfxAlter@, because the effect is obvious (e.g., opened door).
          toTile <- rndToAction $ fromMaybe (assert `failure` tgroup)
                                  <$> opick tgroup (const True)
          unless (toTile == serverTile) $ do
            execUpdAtomic $ UpdAlterTile lid tpos serverTile toTile
            case (Tile.isExplorable coTileSpeedup serverTile,
                  Tile.isExplorable coTileSpeedup toTile) of
              (False, True) -> execUpdAtomic $ UpdAlterClear lid 1
              (True, False) -> execUpdAtomic $ UpdAlterClear lid (-1)
              _ -> return ()
        feats = case mfeat of
          Nothing -> TK.tfeature $ okind serverTile
          Just feat2 | Tile.hasFeature cotile feat2 serverTile -> [feat2]
          Just _ -> []  -- client was wrong about tile features
        toAlter feat =
          case feat of
            TK.OpenTo tgroup -> Just tgroup
            TK.CloseTo tgroup -> Just tgroup
            TK.ChangeTo tgroup -> Just tgroup
            _ -> Nothing
        groupsToAlterTo = mapMaybe toAlter feats
        effs = Tile.listCauseEffects cotile serverTile
    if null groupsToAlterTo && null effs && serverTile == freshClientTile then
      -- Neither searching nor altering possible; silly client.
      execFailure source req AlterNothing
    else
      if EM.notMember tpos $ lfloor lvl then
        if null (posToAidsLvl tpos lvl) then do
          when (serverTile /= freshClientTile) $
            -- Search, in case some actors (of other factions?)
            -- don't know this tile.
            execUpdAtomic $ UpdSearchTile source tpos freshClientTile serverTile
          when (alterSkill >= Tile.alterMinSkill coTileSpeedup serverTile) $ do
            maybe (return ()) changeTo $ listToMaybe groupsToAlterTo
              -- TODO: pick another, if the first one void
            -- Perform effects, if any permitted.
            mapM_ (itemEffectCause source tpos) effs
        else execFailure source req AlterBlockActor
      else execFailure source req AlterBlockItem

-- * ReqWait

-- | Do nothing.
--
-- Something is sometimes done in 'setBWait'.
reqWait :: MonadAtomic m => ActorId -> m ()
{-# INLINE reqWait #-}
reqWait _ = return ()

-- * ReqMoveItems

reqMoveItems :: (MonadAtomic m, MonadServer m)
             => ActorId -> [(ItemId, Int, CStore, CStore)] -> m ()
{-# INLINABLE reqMoveItems #-}
reqMoveItems aid l = do
  b <- getsState $ getActorBody aid
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! aid
  -- Server accepts item movement based on calm at the start, not end
  -- or in the middle, to avoid interrupted or partially ignored commands.
      calmE = calmEnough b ar
  mapM_ (reqMoveItem aid calmE) l

reqMoveItem :: (MonadAtomic m, MonadServer m)
            => ActorId -> Bool -> (ItemId, Int, CStore, CStore) -> m ()
{-# INLINABLE reqMoveItem #-}
reqMoveItem aid calmE (iid, k, fromCStore, toCStore) = do
  b <- getsState $ getActorBody aid
  let fromC = CActor aid fromCStore
      req = ReqMoveItems [(iid, k, fromCStore, toCStore)]
  toC <- case toCStore of
    CGround -> pickDroppable aid b
    _ -> return $! CActor aid toCStore
  bagBefore <- getsState $ getContainerBag toC
  if
   | k < 1 || fromCStore == toCStore -> execFailure aid req ItemNothing
   | toCStore == CEqp && eqpOverfull b k ->
     execFailure aid req EqpOverfull
   | (fromCStore == CSha || toCStore == CSha) && not calmE ->
     execFailure aid req ItemNotCalm
   | otherwise -> do
    itemToF <- itemToFullServer
    let itemFull = itemToF iid (k, [])
    when (fromCStore == CGround) $ do
      case itemFull of
        ItemFull{itemDisco=
                   Just ItemDisco{itemKind=IK.ItemKind{IK.ieffects}}}
          | not $ null $ filter IK.properEffect ieffects ->
          return ()  -- discover by use
        ItemFull{itemBase} -> do
          seed <- getsServer $ (EM.! iid) . sitemSeedD
          Level{ldepth} <- getLevel $ jlid itemBase
          execUpdAtomic $ UpdDiscoverSeed fromC iid seed ldepth
    upds <- generalMoveItem True iid k fromC toC
    mapM_ execUpdAtomic upds
    -- Reset timeout for equipped periodic items.
    when (toCStore `elem` [CEqp, COrgan]
          && fromCStore `notElem` [CEqp, COrgan]) $ do
      localTime <- getsState $ getLocalTime (blid b)
      -- The first recharging period after pick up is random,
      -- between 1 and 2 standard timeouts of the item.
      mrndTimeout <- rndToAction $ computeRndTimeout localTime iid itemFull
      let beforeIt = case iid `EM.lookup` bagBefore of
            Nothing -> []  -- no such items before move
            Just (_, it2) -> it2
      -- The moved item set (not the whole stack) has its timeout
      -- reset to a random value between timeout and twice timeout.
      -- This prevents micromanagement via swapping items in and out of eqp
      -- and via exact prediction of first timeout after equip.
      case mrndTimeout of
        Just rndT -> do
          bagAfter <- getsState $ getContainerBag toC
          let afterIt = case iid `EM.lookup` bagAfter of
                Nothing -> assert `failure` (iid, bagAfter, toC)
                Just (_, it2) -> it2
              resetIt = beforeIt ++ replicate k rndT
          when (afterIt /= resetIt) $
            execUpdAtomic $ UpdTimeItem iid toC afterIt resetIt
        Nothing -> return ()  -- no Periodic or Timeout aspect; don't touch

computeRndTimeout :: Time -> ItemId -> ItemFull -> Rnd (Maybe Time)
{-# INLINABLE computeRndTimeout #-}
computeRndTimeout localTime iid ItemFull{..}= do
  case itemDisco of
    Just ItemDisco{itemKind, itemAspect=Just ar} ->
      case aTimeout ar of
        t | t /= 0 && IK.Periodic `elem` IK.ieffects itemKind -> do
          rndT <- randomR (0, t)
          let rndTurns = timeDeltaScale (Delta timeTurn) rndT
          return $ Just $ timeShift localTime rndTurns
        _ -> return Nothing
    _ -> assert `failure` iid

-- * ReqProject

reqProject :: (MonadAtomic m, MonadServer m)
           => ActorId    -- ^ actor projecting the item (is on current lvl)
           -> Point      -- ^ target position of the projectile
           -> Int        -- ^ digital line parameter
           -> ItemId     -- ^ the item to be projected
           -> CStore     -- ^ whether the items comes from floor or inventory
           -> m ()
{-# INLINABLE reqProject #-}
reqProject source tpxy eps iid cstore = do
  let req = ReqProject tpxy eps iid cstore
  b <- getsState $ getActorBody source
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! source
      calmE = calmEnough b ar
  if cstore == CSha && not calmE then execFailure source req ItemNotCalm
  else do
    mfail <- projectFail source tpxy eps iid cstore False
    maybe (return ()) (execFailure source req) mfail

-- * ReqApply

reqApply :: (MonadAtomic m, MonadServer m)
         => ActorId  -- ^ actor applying the item (is on current level)
         -> ItemId   -- ^ the item to be applied
         -> CStore   -- ^ the location of the item
         -> m ()
{-# INLINABLE reqApply #-}
reqApply aid iid cstore = do
  let req = ReqApply iid cstore
  b <- getsState $ getActorBody aid
  actorAspect <- getsServer sactorAspect
  let ar = actorAspect EM.! aid
      calmE = calmEnough b ar
  if cstore == CSha && not calmE then execFailure aid req ItemNotCalm
  else do
    bag <- getsState $ getBodyStoreBag b cstore
    case EM.lookup iid bag of
      Nothing -> execFailure aid req ApplyOutOfReach
      Just kit -> do
        itemToF <- itemToFullServer
        actorSk <- actorSkillsServer aid
        localTime <- getsState $ getLocalTime (blid b)
        let skill = EM.findWithDefault 0 Ability.AbApply actorSk
            itemFull = itemToF iid kit
            legal = permittedApply localTime skill b ar " " itemFull
        case legal of
          Left reqFail -> execFailure aid req reqFail
          Right _ -> applyItem aid iid cstore

-- * ReqGameRestart

-- TODO: implement a handshake and send hero names there,
-- so that they are available in the first game too,
-- not only in subsequent, restarted, games.
reqGameRestart :: (MonadAtomic m, MonadServer m)
               => ActorId -> GroupName ModeKind -> Int -> [(Int, (Text, Text))]
               -> m ()
{-# INLINABLE reqGameRestart #-}
reqGameRestart aid groupName d configHeroNames = do
  modifyServer $ \ser -> ser {sdebugNxt = (sdebugNxt ser) {scurDiffSer = d}}
  b <- getsState $ getActorBody aid
  oldSt <- getsState $ gquit . (EM.! bfid b) . sfactionD
  modifyServer $ \ser ->
    ser { swriteSave = True  -- fake saving to abort turn
        , squit = True  -- do this at once
        , sheroNames = EM.insert (bfid b) configHeroNames $ sheroNames ser }
  isNoConfirms <- isNoConfirmsGame
  -- This call to `revealItems` is really needed, because the other
  -- happens only at game conclusion, not at quitting.
  unless isNoConfirms $ revealItems Nothing
  execUpdAtomic $ UpdQuitFaction (bfid b) oldSt
                $ Just $ Status Restart (fromEnum $ blid b) (Just groupName)

-- * ReqGameExit

reqGameExit :: (MonadAtomic m, MonadServer m) => ActorId -> m ()
{-# INLINABLE reqGameExit #-}
reqGameExit aid = do
  b <- getsState $ getActorBody aid
  oldSt <- getsState $ gquit . (EM.! bfid b) . sfactionD
  modifyServer $ \ser -> ser { swriteSave = True
                             , squit = True }  -- do this at once
  execUpdAtomic $ UpdQuitFaction (bfid b) oldSt
                $ Just $ Status Camping (fromEnum $ blid b) Nothing

-- * ReqGameSave

reqGameSave :: MonadServer m => m ()
{-# INLINABLE reqGameSave #-}
reqGameSave = do
  modifyServer $ \ser -> ser { swriteSave = True
                             , squit = True }  -- do this at once

-- * ReqTactic

reqTactic :: MonadAtomic m => FactionId -> Tactic -> m ()
{-# INLINABLE reqTactic #-}
reqTactic fid toT = do
  fromT <- getsState $ ftactic . gplayer . (EM.! fid) . sfactionD
  execUpdAtomic $ UpdTacticFaction fid toT fromT

-- * ReqAutomate

reqAutomate :: MonadAtomic m => FactionId -> m ()
{-# INLINABLE reqAutomate #-}
reqAutomate fid = execUpdAtomic $ UpdAutoFaction fid True
