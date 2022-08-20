{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Domain.Missions.Specifications (isAquatic, isAero, isAeroAquatic, specifiedMission, specifiedMissions) where

import Data.Kind ( Type, Constraint )
import Domain.Missions.Mission (Mission(..), MissionInfo(..))

data Specs = IsAquatic | IsAero | IsAeroAquatic deriving Show

type SpecifiedMission :: [Specs] -> Type
data SpecifiedMission specifications where SpecifiedMission :: Mission (MissionInfo spy value) state -> SpecifiedMission specifications

instance Show (SpecifiedMission specifications) where
  show (SpecifiedMission mission) = show mission

specifiedMission :: Mission (MissionInfo spy value) state -> SpecifiedMission '[]
specifiedMission = SpecifiedMission

specifiedMissions :: [Mission (MissionInfo spy value) state] -> [SpecifiedMission '[]]
specifiedMissions = foldr (\ spy -> (<>) [specifiedMission spy]) []

isAquatic :: SpecifiedMission specs -> Maybe(SpecifiedMission (IsAquatic ': specs))
isAquatic (SpecifiedMission mission) = case mission of 
  MkMission info state -> case info of
      AquaticInfo _ _ -> Just $ SpecifiedMission mission
      _ -> Nothing

isAero :: SpecifiedMission specs -> Maybe(SpecifiedMission (IsAero ': specs))
isAero (SpecifiedMission mission) = case mission of 
    MkMission info state -> case info of
      AeroInfo _ _ -> Just $ SpecifiedMission mission
      _ -> Nothing

isAeroAquatic :: SpecifiedMission specs -> Maybe(SpecifiedMission (IsAeroAquatic ': specs))
isAeroAquatic (SpecifiedMission mission) = case mission of 
    MkMission info state -> case info of
      AeroAquaticInfo _ _ -> Just $ SpecifiedMission mission
      _ -> Nothing 
