{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Domain.Missions.Mission (mkAquaticMission, mkAeroMission, mkAeroAquaticMission, mkInvalidMission, transition, execute, complete, Mission(..), MissionInfo(..), State(..)) where

import Data.Kind (Type)
import Common.Operators (Satisfied, Expression(Spec), And)
import Domain.Spies.Specifications.SpySpecification
    ( SpecifiedSpy (SpecifiedSpy), Specs(Do), CanDo(AquaticMission, AeroMission) )
import Domain.Spies.Spy(Spy (name))

data State = Pending | Execution | Executed | Invalided deriving Show
newtype Transition actual final = Transition (State -> State)

type MissionInfo :: Type -> Type -> Type
data MissionInfo spy value where
  AquaticInfo :: (Satisfied (Spec (Do AquaticMission)) specs, Floating value) => {
    _spy :: SpecifiedSpy specs,
    _value :: value
  } -> MissionInfo spy value

  AeroInfo :: (Satisfied (Spec (Do AeroMission)) specs, Floating value) => {
    _spy :: SpecifiedSpy specs,
    _value :: value
  } -> MissionInfo spy value

  AeroAquaticInfo :: (Satisfied (And (Spec (Do AeroMission)) (Spec (Do AquaticMission))) specs, Floating value) => {
    _spy :: SpecifiedSpy specs,
    _value :: value
  } -> MissionInfo spy value

  MkInvalidMission :: MissionInfo spy value


type Mission :: Type -> Type -> Type
data Mission info state where
  MkMission :: MissionInfo spy value -> State -> Mission info state

instance Show (Mission info state) where
  show (MkMission MkInvalidMission state) = "Invalid mission"
  show (MkMission (AquaticInfo (SpecifiedSpy spy) value) state) = "Aquatic Mission for " ++ name spy ++ " " ++ show state
  show (MkMission (AeroInfo (SpecifiedSpy spy) value) state) = "Aero Mission for " ++ name spy ++ " " ++ show state
  show (MkMission (AeroAquaticInfo (SpecifiedSpy spy) value) state) = "Aero-Aquatic Mission for " ++ name spy ++ " " ++ show state

mkInvalidMission :: Mission (MissionInfo Spy value) State
mkInvalidMission = MkMission MkInvalidMission Invalided

mkAquaticMission :: (Satisfied (Spec (Do AquaticMission)) specs, Floating value) => SpecifiedSpy specs -> value -> Mission(MissionInfo Spy value) State
mkAquaticMission specified value = MkMission (AquaticInfo specified value) Pending

mkAeroMission :: (Satisfied (Spec (Do AeroMission)) specs, Floating value) => SpecifiedSpy specs -> value -> Mission(MissionInfo Spy value) State
mkAeroMission specified value = MkMission (AeroInfo specified value) Pending

mkAeroAquaticMission :: (Satisfied (And (Spec (Do AeroMission)) (Spec (Do AquaticMission))) specs, Floating value) => SpecifiedSpy specs -> value -> Mission(MissionInfo Spy value) State
mkAeroAquaticMission specified value = MkMission (AeroAquaticInfo specified value) Pending

transition :: Transition actual final -> Mission mission actual -> Mission mission final
transition (Transition func) (MkMission mission state) = MkMission mission (func state)

execute :: Transition Pending Execution
execute = Transition id

complete :: Transition Execution Executed
complete = Transition id