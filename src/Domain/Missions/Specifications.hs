{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Domain.Missions.Specifications (isAquatic, isAero, isAeroAquatic, specifiedMission, specifiedMissions, isPending, SpecifiedMission(..), Specs(..)) where

import Data.Kind ( Type, Constraint )
import Domain.Missions.Mission (Mission(..), MissionInfo(..), State(..))

data Specs = IsAquatic | IsAero | IsAeroAquatic deriving Show

type SpecifiedMission :: [Specs] -> State -> Type
data SpecifiedMission specifications state where 
  SpecifiedMission :: Mission (MissionInfo spy value) state -> SpecifiedMission specifications state

instance Show (SpecifiedMission specifications state) where
  show (SpecifiedMission mission) = show mission

specifiedMission :: Mission (MissionInfo spy value) state -> SpecifiedMission '[] state
specifiedMission = SpecifiedMission

specifiedMissions :: [Mission (MissionInfo spy value) state] -> [SpecifiedMission '[] state]
specifiedMissions = foldr (\ spy -> (<>) [specifiedMission spy]) []

isAquatic :: SpecifiedMission specs state -> Maybe(SpecifiedMission (IsAquatic ': specs) state)
isAquatic (SpecifiedMission mission) = case mission of 
  MkMission info state -> case info of
      AquaticInfo _ _ -> Just $ SpecifiedMission mission
      _ -> Nothing

isAero :: SpecifiedMission specs state -> Maybe(SpecifiedMission (IsAero ': specs) state)
isAero (SpecifiedMission mission) = case mission of 
    MkMission info state -> case info of
      AeroInfo _ _ -> Just $ SpecifiedMission mission
      _ -> Nothing

isAeroAquatic :: SpecifiedMission specs state -> Maybe(SpecifiedMission (IsAeroAquatic ': specs) state)
isAeroAquatic (SpecifiedMission mission) = case mission of 
    MkMission info state -> case info of
      AeroAquaticInfo _ _ -> Just $ SpecifiedMission mission
      _ -> Nothing 

isPending :: SpecifiedMission specs state -> Maybe(SpecifiedMission specs Pending)
isPending (SpecifiedMission mission) = case mission of 
  (MkMission info Pending) -> Just $ SpecifiedMission (MkMission info Pending)
  (MkMission _ _) -> Nothing