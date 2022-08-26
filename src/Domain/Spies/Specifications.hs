{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}


module Domain.Spies.Specifications (
    SpecifiedSpy, CanDrive(..), CanDo(..), Specs(..), 
    specifiedSpy, specifiedSpies,
    canDriveCar, canPilotBoat, canPilotHelicopter,
    canDoAeroMission, canDoAquaticMission,
    withSpy) where
import Data.Kind ( Type, Constraint )
import Domain.Spies.Spy ( Spy, age )
import Common.Operators (Satisfied, And, Or, Expression(Spec), NoT)

data CanDrive    = Car | Boat | Helicopter   deriving Show
data CanDo       = AquaticMission | AeroMission deriving Show
data Specs a = Drive CanDrive | Do CanDo deriving Show

type SpecifiedSpy :: [Specs a] -> Type
data SpecifiedSpy specifications where SpecifiedSpy :: Spy -> SpecifiedSpy specifications deriving Show

specifiedSpy :: Spy -> SpecifiedSpy '[]
specifiedSpy = SpecifiedSpy

specifiedSpies :: [Spy] -> [SpecifiedSpy '[]]
specifiedSpies = foldr (\ spy -> (<>) [specifiedSpy spy]) []

withSpy :: SpecifiedSpy specs -> (Spy -> return) -> return
withSpy (SpecifiedSpy spy) f = f spy

canPilotBoat :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (Drive Boat ':specifications))
canPilotBoat (SpecifiedSpy spy) =
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing

canDriveCar :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (Drive Car ': specifications))
canDriveCar (SpecifiedSpy spy) =
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing

canPilotHelicopter :: SpecifiedSpy specifications -> Maybe(SpecifiedSpy (Drive Helicopter ': specifications))
canPilotHelicopter (SpecifiedSpy spy) = 
    if age spy >= 18 then Just $ SpecifiedSpy spy
    else Nothing

canDoAquaticMission :: Satisfied (And (Spec (Drive Car)) (Spec (Drive Boat))) specs => SpecifiedSpy specs -- ^ 
  -> Maybe (SpecifiedSpy (Do AquaticMission ': specs))
canDoAquaticMission (SpecifiedSpy spy) = 
    if age spy >= 35 then Just $ SpecifiedSpy spy
    else Nothing

canDoAeroMission :: Satisfied (Or (Spec (Drive Helicopter)) (NoT (Spec (Drive Car)))) specs => SpecifiedSpy  specs -- ^ 
  -> Maybe(SpecifiedSpy (Do AeroMission ': specs))
canDoAeroMission (SpecifiedSpy spy) = 
    if age spy >= 35 then Just $ SpecifiedSpy spy
    else Nothing