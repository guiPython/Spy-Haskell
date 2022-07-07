{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Spies where
import Data.Kind (Type)

data SpySpecifications = CanDriveCar | CanFlyPlane | CanDriveMotorCycle | CanPilotBoat | CanPilotJetSki

data Spy = Spy {
  name :: String,
  age  :: Int
} deriving  Show

type SpecifiedSpy :: [SpySpecifications] -> Type
data SpecifiedSpy specifications where SpecifiedSpy :: Spy -> SpecifiedSpy specifications deriving Show

specifiedSpy :: Spy -> SpecifiedSpy '[]
specifiedSpy = SpecifiedSpy

canPilotJetSki :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (CanPilotJetSki ': specifications))
canPilotJetSki (SpecifiedSpy spy) =
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing

canPilotBoat :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (CanPilotBoat ': specifications))
canPilotBoat (SpecifiedSpy spy) =
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing

canDoWaterMissions :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (CanPilotBoat ': CanPilotJetSki ': specifications))
canDoWaterMissions specifiedSpy = canPilotJetSki specifiedSpy >>= canPilotBoat

applySpecification :: SpecifiedSpy a -> (SpecifiedSpy a -> SpecifiedSpy b) -> SpecifiedSpy b
applySpecification spy specification = specification spy

specifiedSpies :: [Spy] -> [SpecifiedSpy '[]]
specifiedSpies = foldr (\ spy -> (<>) [specifiedSpy spy]) []