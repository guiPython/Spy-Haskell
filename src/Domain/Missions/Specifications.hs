{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module Domain.Missions.Specifications (isAquatic, isAero, isAeroAquatic, specifiedMission, specifiedMissions, isPending, SpecifiedMission, Specs(..)) where

import Data.Kind ( Type, Constraint )
import Domain.Missions.Mission (Mission, MissionInfo(..), State(..), withSomeMissionState, withSomeMissionInfo, mkAquaticMission, mkAeroAquaticMission, mkAeroMission, mkInvalidMission)
import Domain.Spies.Specifications (CanDo(AquaticMission))

data Specs = IsAquatic | IsAero | IsAeroAquatic deriving Show

type SpecifiedMission :: [Specs] -> State -> Type
data SpecifiedMission specifications state where 
  SpecifiedMission :: Mission info state -> SpecifiedMission specifications state

instance Show (SpecifiedMission specifications state) where
  show (SpecifiedMission mission) = show mission

specifiedMission :: Mission info state -> SpecifiedMission '[] state
specifiedMission = SpecifiedMission

specifiedMissions :: [Mission info state] -> [SpecifiedMission '[] state]
specifiedMissions = foldr (\ spy -> (<>) [specifiedMission spy]) []

isAquatic :: SpecifiedMission specs state -> Maybe(SpecifiedMission (IsAquatic ': specs) state)
isAquatic (SpecifiedMission mission) = withSomeMissionInfo mission (\case
    AquaticInfo _ _ -> Just $ SpecifiedMission mission
    _ -> Nothing
  )

isAero :: SpecifiedMission specs state -> Maybe(SpecifiedMission (IsAero ': specs) state)
isAero (SpecifiedMission mission) = withSomeMissionInfo mission (\case
    AeroInfo _ _ -> Just $ SpecifiedMission mission
    _ -> Nothing
  )

isAeroAquatic :: SpecifiedMission specs state -> Maybe(SpecifiedMission (IsAeroAquatic ': specs) state)
isAeroAquatic (SpecifiedMission mission) = withSomeMissionInfo mission (\case
    AeroAquaticInfo _ _ -> Just $ SpecifiedMission mission
    _ -> Nothing
  )

isPending :: SpecifiedMission specs state -> Maybe(SpecifiedMission specs Pending)
isPending (SpecifiedMission mission) = withSomeMissionState mission (\case 
  Pending -> withSomeMissionInfo mission (\case
    AeroAquaticInfo spy value -> Just $ SpecifiedMission $ mkAeroAquaticMission spy value
    AquaticInfo spy value -> Just $ SpecifiedMission $ mkAquaticMission spy value
    AeroInfo spy value -> Just $ SpecifiedMission $ mkAeroMission spy value
    InvalidMissionInfo -> Just $ SpecifiedMission mkInvalidMission
    )
  _ -> Nothing
  )