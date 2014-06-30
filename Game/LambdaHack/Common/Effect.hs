{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
-- | Effects of content on the game state. No operation in this module
-- involves state or monad types.
module Game.LambdaHack.Common.Effect
  ( Effect(..), Aspect(..), ThrowMod(..), Feature(..), EqpSlot(..)
  , effectTrav, aspectTrav
  ) where

import qualified Control.Monad.State as St
import Data.Binary
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Game.LambdaHack.Common.Ability as Ability
import qualified Game.LambdaHack.Common.Dice as Dice

-- TODO: document each constructor
-- | Effects of items. Can be invoked by the item wielder to affect
-- another actor or the wielder himself. Many occurences in the same item
-- are possible.
data Effect a =
    NoEffect
  | Hurt !Dice.Dice
  | Burn !Int
  | Explode !Text        -- ^ explode, producing this group of shrapnel
  | RefillHP !Int
  | RefillCalm !Int
  | Dominate
  | Impress
  | CallFriend !Int
  | Summon !a
  | CreateItem !Int
  | Ascend !Int
  | Escape !Int          -- ^ the arg tells if can be placed on last level, etc.
  | Paralyze !a
  | InsertMove !a
  | Teleport !a
  | SendFlying !ThrowMod
  | PushActor !ThrowMod
  | PullActor !ThrowMod
  | DropBestWeapon
  | DropEqp !Char !Bool  -- ^ symbol @' '@ means all, @True@ means hit on drop
  | ActivateEqp !Char    -- ^ symbol @' '@ means all
  | ApplyPerfume
  | OnSmash !(Effect a)  -- ^ trigger when item smashed (not applied nor meleed)
  | TimedAspect !Int !(Aspect a)  -- ^ enable the aspect for k clips
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Aspects of items. Additive (starting at 0) for all items wielded
-- by an actor and affect the actor (except @Periodic@ that only affect
-- the item and so is not additive).
data Aspect a =
    Periodic !a        -- ^ is activated this many times in 100
  | AddHurtMelee !a    -- ^ percentage damage bonus against melee
  | AddArmorMelee !a   -- ^ percentage armor bonus against melee
  | AddHurtRanged !a   -- ^ percentage damage bonus against ranged
  | AddArmorRanged !a  -- ^ percentage armor bonus against ranged
  | AddMaxHP !a        -- ^ maximal hp
  | AddMaxCalm !a      -- ^ maximal calm
  | AddSpeed !a        -- ^ speed in m/10s
  | AddSkills !Ability.Skills  -- ^ skills in particular abilities
  | AddSight !a        -- ^ FOV radius, where 1 means a single tile
  | AddSmell !a        -- ^ smell radius, where 1 means a single tile
  | AddLight !a        -- ^ light radius, where 1 means a single tile
  deriving (Show, Read, Eq, Ord, Generic, Functor)

-- | Parameters modifying a throw. Not additive and don't start at 0.
data ThrowMod = ThrowMod
  { throwVelocity :: !Int  -- ^ fly with this percentage of base throw speed
  , throwLinger   :: !Int  -- ^ fly for this percentage of 2 turns
  }
  deriving (Show, Read, Eq, Ord, Generic)

-- | Features of item. Affect only the item in question, not the actor,
-- and so not additive in any sense.
data Feature =
    ChangeTo !Text           -- ^ change to this group when altered
  | Fragile                  -- ^ break even when not hitting an enemy
  | Durable                  -- ^ don't break even hitting or applying
  | ToThrow !ThrowMod        -- ^ parameters modifying a throw
  | Applicable               -- ^ can't be turned off, is consumed by use
  | EqpSlot !EqpSlot !Text   -- ^ the slot, counts towards the eqp limit
  | Identified               -- ^ any such item starts identified
  | Precious                 -- ^ precious; don't risk identifying by use
  deriving (Show, Eq, Ord, Generic)

data EqpSlot =
    EqpSlotPeriodic
  | EqpSlotAddMaxHP
  | EqpSlotAddMaxCalm
  | EqpSlotAddSpeed
  | EqpSlotAbility
  | EqpSlotAddHurtMelee
  | EqpSlotAddHurtRanged
  | EqpSlotAddArmorMelee
  | EqpSlotAddArmorRanged
  | EqpSlotAddSight
  | EqpSlotAddSmell
  | EqpSlotAddLight
  | EqpSlotWeapon
  deriving (Show, Eq, Ord, Generic)

instance Hashable a => Hashable (Effect a)

instance Hashable a => Hashable (Aspect a)

instance Hashable ThrowMod

instance Hashable Feature

instance Hashable EqpSlot

instance Binary a => Binary (Effect a)

instance Binary a => Binary (Aspect a)

instance Binary ThrowMod

instance Binary Feature

instance Binary EqpSlot

-- TODO: Traversable?
-- | Transform an effect using a stateful function.
effectTrav :: Effect a -> (a -> St.State s b) -> St.State s (Effect b)
effectTrav NoEffect _ = return NoEffect
effectTrav (RefillHP p) _ = return $! RefillHP p
effectTrav (Hurt dice) _ = return $! Hurt dice
effectTrav (RefillCalm p) _ = return $! RefillCalm p
effectTrav Dominate _ = return Dominate
effectTrav Impress _ = return Impress
effectTrav (CallFriend p) _ = return $! CallFriend p
effectTrav (Summon a) f = do
  b <- f a
  return $! Summon b
effectTrav (CreateItem p) _ = return $! CreateItem p
effectTrav ApplyPerfume _ = return ApplyPerfume
effectTrav (Burn p) _ = return $! Burn p
effectTrav (Ascend p) _ = return $! Ascend p
effectTrav (Escape p) _ = return $! Escape p
effectTrav (Paralyze a) f = do
  b <- f a
  return $! Paralyze b
effectTrav (InsertMove a) f = do
  b <- f a
  return $! InsertMove b
effectTrav DropBestWeapon _ = return DropBestWeapon
effectTrav (DropEqp symbol hit) _ = return $! DropEqp symbol hit
effectTrav (SendFlying tmod) _ = return $! SendFlying tmod
effectTrav (PushActor tmod) _ = return $! PushActor tmod
effectTrav (PullActor tmod) _ = return $! PullActor tmod
effectTrav (Teleport a) f = do
  b <- f a
  return $! Teleport b
effectTrav (ActivateEqp symbol) _ = return $! ActivateEqp symbol
effectTrav (OnSmash effa) f = do
  effb <- effectTrav effa f
  return $! OnSmash effb
effectTrav (Explode t) _ = return $! Explode t
effectTrav (TimedAspect k asp) f = do
  asp2 <- aspectTrav asp f
  return $! TimedAspect k asp2

-- | Transform an aspect using a stateful function.
aspectTrav :: Aspect a -> (a -> St.State s b) -> St.State s (Aspect b)
aspectTrav (Periodic a) f = do
  b <- f a
  return $! Periodic b
aspectTrav (AddMaxHP a) f = do
  b <- f a
  return $! AddMaxHP b
aspectTrav (AddMaxCalm a) f = do
  b <- f a
  return $! AddMaxCalm b
aspectTrav (AddSpeed a) f = do
  b <- f a
  return $! AddSpeed b
aspectTrav (AddSkills as) _ = return $! AddSkills as
aspectTrav (AddHurtMelee a) f = do
  b <- f a
  return $! AddHurtMelee b
aspectTrav (AddHurtRanged a) f = do
  b <- f a
  return $! AddHurtRanged b
aspectTrav (AddArmorMelee a) f = do
  b <- f a
  return $! AddArmorMelee b
aspectTrav (AddArmorRanged a) f = do
  b <- f a
  return $! AddArmorRanged b
aspectTrav (AddSight a) f = do
  b <- f a
  return $! AddSight b
aspectTrav (AddSmell a) f = do
  b <- f a
  return $! AddSmell b
aspectTrav (AddLight a) f = do
  b <- f a
  return $! AddLight b
