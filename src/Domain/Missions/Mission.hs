{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE RankNTypes #-}
module Domain.Missions.Mission (
  mkAquaticMission,
  mkAeroMission, 
  mkAeroAquaticMission, 
  mkInvalidMission, 
  transition, execute, complete,
  value, updateValue,
  withSomeMissionInfo, withSomeMissionState,
  Mission, State(..), MissionInfo(..)) where

import Data.Kind (Type)
import Common.Operators (Satisfied, Expression(Spec), And)
import Domain.Spies.Specifications
    ( SpecifiedSpy, Specs(Do), CanDo(AquaticMission, AeroMission), withSpy )
import Domain.Spies.Spy(Spy (name))

data State = Pending | Execution | Executed | Invalided deriving Show
newtype Transition actual final = Transition (State -> State)

type MissionInfo :: Type -> Type -> Type
data MissionInfo spy value where
  AquaticInfo :: (Satisfied (Spec (Do AquaticMission)) specs) => {
    _spy :: SpecifiedSpy specs,
    _value :: Double
  } -> MissionInfo spy value

  AeroInfo :: (Satisfied (Spec (Do AeroMission)) specs, Num value) => {
    _spy :: SpecifiedSpy specs,
    _value :: Double
  } -> MissionInfo spy value

  AeroAquaticInfo :: (Satisfied (And (Spec (Do AeroMission)) (Spec (Do AquaticMission))) specs) => {
    _spy :: SpecifiedSpy specs,
    _value :: Double
  } -> MissionInfo spy value

  InvalidMissionInfo :: MissionInfo spy value


type ExMission :: Type -> State -> Type
data ExMission info state where
  MkMission :: MissionInfo spy value -> State -> ExMission info state

instance Show (ExMission info state) where
  show (MkMission InvalidMissionInfo state) = "Invalid mission"
  show (MkMission (AquaticInfo specifiedSpy value) state) = "Aquatic Mission for " ++ withSpy specifiedSpy name ++ " " ++ show state
  show (MkMission (AeroInfo specifiedSpy value) state) = "Aero Mission for " ++ withSpy specifiedSpy name ++ " " ++ show state
  show (MkMission (AeroAquaticInfo specifiedSpy value) state) = "Aero-Aquatic Mission for " ++ withSpy specifiedSpy name ++ " " ++ show state

withSomeMissionInfo :: ExMission info state -> (forall s v. MissionInfo s v -> return) -> return
withSomeMissionInfo (MkMission info state) f = f info

withSomeMissionState :: ExMission info state -> (State -> return) -> return
withSomeMissionState (MkMission info state) f = f state

type Mission = ExMission

mkInvalidMission :: Mission (MissionInfo Spy value) Pending
mkInvalidMission = MkMission InvalidMissionInfo Pending

mkAquaticMission :: (Satisfied (Spec (Do AquaticMission)) specs) => SpecifiedSpy specs -> Double -> Mission(MissionInfo Spy Double) Pending
mkAquaticMission specified value = MkMission (AquaticInfo specified value) Pending

mkAeroMission :: (Satisfied (Spec (Do AeroMission)) specs) => SpecifiedSpy specs -> Double -> Mission(MissionInfo Spy Double) Pending
mkAeroMission specified value = MkMission (AeroInfo specified value) Pending

mkAeroAquaticMission :: (Satisfied (And (Spec (Do AeroMission)) (Spec (Do AquaticMission))) specs) => SpecifiedSpy specs -> Double -> Mission(MissionInfo Spy Double) Pending
mkAeroAquaticMission specified value = MkMission (AeroAquaticInfo specified value) Pending

value :: Mission (MissionInfo Spy Double) Pending -> Double
value (MkMission (AeroInfo _ v) _) = v
value (MkMission (AquaticInfo _ v) _) = v
value (MkMission (AeroAquaticInfo spy v) _) = v 
value (MkMission InvalidMissionInfo _) = 0 

updateValue :: Mission (MissionInfo Spy Double) Pending -> Double -> Mission (MissionInfo Spy Double) Pending
updateValue (MkMission (AeroInfo spy _) _) value = mkAeroMission spy value
updateValue (MkMission (AquaticInfo spy _) _) value = mkAquaticMission spy value
updateValue (MkMission (AeroAquaticInfo spy _) _) value = mkAeroAquaticMission spy value

transition :: Transition actual final -> Mission mission actual -> Mission mission final
transition (Transition func) (MkMission mission state) = MkMission mission (func state)

execute :: Transition Pending Execution
execute = Transition id

complete :: Transition Execution Executed
complete = Transition id